;; Blood Type Compatibility Registry
;; Emergency blood type matching and donor availability system

(define-constant ERR-NOT-AUTHORIZED (err u700))
(define-constant ERR-DONOR-NOT-REGISTERED (err u701))
(define-constant ERR-INVALID-BLOOD-TYPE (err u702))
(define-constant ERR-ALERT-NOT-FOUND (err u703))
(define-constant ERR-ALREADY-REGISTERED (err u704))
(define-constant ERR-INVALID-URGENCY-LEVEL (err u705))

;; Data variables
(define-data-var next-alert-id uint u1)

;; Map to store donor blood type information
(define-map donor-blood-types
    principal
    {
        blood-type: (string-ascii 5),
        rh-factor: bool,
        available-for-emergency: bool,
        last-donation-date: uint,
        medical-restrictions: (string-ascii 200),
        emergency-contact: (optional (string-ascii 50)),
        registered-at: uint
    }
)

;; Map to track emergency alerts
(define-map emergency-alerts
    uint
    {
        blood-type-needed: (string-ascii 5),
        rh-factor-needed: bool,
        urgency-level: uint,
        hospital: principal,
        units-needed: uint,
        units-found: uint,
        location: (string-ascii 100),
        contact-info: (string-ascii 100),
        created-at: uint,
        expires-at: uint,
        active: bool
    }
)

;; Map to track alert responses
(define-map alert-responses
    { alert-id: uint, donor: principal }
    {
        response-time: uint,
        available: bool,
        estimated-arrival: uint
    }
)

;; Map to store blood type compatibility rules
(define-map blood-compatibility
    (string-ascii 5)
    (list 10 (string-ascii 5))
)

;; Read-only functions

(define-read-only (get-donor-blood-info (donor principal))
    (map-get? donor-blood-types donor)
)

(define-read-only (get-emergency-alert (alert-id uint))
    (map-get? emergency-alerts alert-id)
)

(define-read-only (get-alert-response (alert-id uint) (donor principal))
    (map-get? alert-responses { alert-id: alert-id, donor: donor })
)

(define-read-only (get-compatible-blood-types (blood-type (string-ascii 5)))
    (default-to (list) (map-get? blood-compatibility blood-type))
)

;; Public functions

(define-public (register-blood-type
    (blood-type (string-ascii 5))
    (rh-factor bool)
    (available-for-emergency bool)
    (medical-restrictions (string-ascii 200))
    (emergency-contact (optional (string-ascii 50))))
    (let (
        (existing-registration (map-get? donor-blood-types tx-sender))
        (donor-check (contract-call? .DonorX get-donor-info tx-sender))
    )
        ;; Check if donor is registered in main DonorX contract
        (asserts! (is-ok donor-check) ERR-DONOR-NOT-REGISTERED)
        (asserts! (is-some (unwrap-panic donor-check)) ERR-DONOR-NOT-REGISTERED)
        (asserts! (is-none existing-registration) ERR-ALREADY-REGISTERED)
        (asserts! (is-valid-blood-type blood-type) ERR-INVALID-BLOOD-TYPE)
        
        (map-set donor-blood-types tx-sender {
            blood-type: blood-type,
            rh-factor: rh-factor,
            available-for-emergency: available-for-emergency,
            last-donation-date: u0,
            medical-restrictions: medical-restrictions,
            emergency-contact: emergency-contact,
            registered-at: stacks-block-height
        })
        
        (ok true)
    )
)

(define-public (update-availability-status (available bool))
    (let (
        (current-info (unwrap! (map-get? donor-blood-types tx-sender) ERR-DONOR-NOT-REGISTERED))
    )
        (map-set donor-blood-types tx-sender
            (merge current-info { available-for-emergency: available })
        )
        (ok true)
    )
)

(define-public (create-emergency-alert
    (blood-type-needed (string-ascii 5))
    (rh-factor-needed bool)
    (urgency-level uint)
    (units-needed uint)
    (location (string-ascii 100))
    (contact-info (string-ascii 100))
    (duration-hours uint))
    (let (
        (alert-id (var-get next-alert-id))
        (current-block stacks-block-height)
        (expires-at (+ current-block (* duration-hours u6)))
    )
        ;; Only authorized donation centers can create alerts
        (asserts! (contract-call? .DonorX is-donation-center tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-valid-blood-type blood-type-needed) ERR-INVALID-BLOOD-TYPE)
        (asserts! (and (>= urgency-level u1) (<= urgency-level u5)) ERR-INVALID-URGENCY-LEVEL)
        (asserts! (> units-needed u0) ERR-INVALID-URGENCY-LEVEL)
        
        (map-set emergency-alerts alert-id {
            blood-type-needed: blood-type-needed,
            rh-factor-needed: rh-factor-needed,
            urgency-level: urgency-level,
            hospital: tx-sender,
            units-needed: units-needed,
            units-found: u0,
            location: location,
            contact-info: contact-info,
            created-at: current-block,
            expires-at: expires-at,
            active: true
        })
        
        (var-set next-alert-id (+ alert-id u1))
        (ok alert-id)
    )
)

(define-public (respond-to-alert
    (alert-id uint)
    (available bool)
    (estimated-arrival uint))
    (let (
        (alert-info (unwrap! (map-get? emergency-alerts alert-id) ERR-ALERT-NOT-FOUND))
        (donor-info (unwrap! (map-get? donor-blood-types tx-sender) ERR-DONOR-NOT-REGISTERED))
        (current-block stacks-block-height)
    )
        (asserts! (get active alert-info) ERR-ALERT-NOT-FOUND)
        (asserts! (< current-block (get expires-at alert-info)) ERR-ALERT-NOT-FOUND)
        (asserts! (can-donate-to-type (get blood-type donor-info) (get blood-type-needed alert-info)) ERR-INVALID-BLOOD-TYPE)
        
        (map-set alert-responses { alert-id: alert-id, donor: tx-sender } {
            response-time: current-block,
            available: available,
            estimated-arrival: estimated-arrival
        })
        
        (if available
            (map-set emergency-alerts alert-id
                (merge alert-info { units-found: (+ (get units-found alert-info) u1) })
            )
            true
        )
        
        (ok true)
    )
)

(define-public (close-emergency-alert (alert-id uint))
    (let (
        (alert-info (unwrap! (map-get? emergency-alerts alert-id) ERR-ALERT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get hospital alert-info)) ERR-NOT-AUTHORIZED)
        (asserts! (get active alert-info) ERR-ALERT-NOT-FOUND)
        
        (map-set emergency-alerts alert-id
            (merge alert-info { active: false })
        )
        (ok true)
    )
)

;; Private functions

(define-private (is-valid-blood-type (blood-type (string-ascii 5)))
    (or
        (is-eq blood-type "A")
        (is-eq blood-type "B")
        (is-eq blood-type "AB")
        (is-eq blood-type "O")
    )
)

(define-private (can-donate-to-type (donor-type (string-ascii 5)) (recipient-type (string-ascii 5)))
    (or
        (is-eq donor-type "O")
        (is-eq donor-type recipient-type)
        (and (is-eq donor-type "A") (is-eq recipient-type "AB"))
        (and (is-eq donor-type "B") (is-eq recipient-type "AB"))
    )
)

;; Initialize compatibility rules
(define-public (initialize-compatibility-rules)
    (begin
        (asserts! (contract-call? .DonorX is-donation-center tx-sender) ERR-NOT-AUTHORIZED)
        
        (map-set blood-compatibility "O" (list "O" "A" "B" "AB"))
        (map-set blood-compatibility "A" (list "A" "AB"))
        (map-set blood-compatibility "B" (list "B" "AB"))
        (map-set blood-compatibility "AB" (list "AB"))
        
        (ok true)
    )
)
