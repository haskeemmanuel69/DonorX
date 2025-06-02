(define-constant contract-name "DonorX")
(define-constant contract-version "1.0.0")
(define-constant contract-description "A smart contract for managing donor badges and records in the DonorX ecosystem.")
(define-constant contract-author "DonorX Team")
(define-constant contract-license "MIT")
(define-non-fungible-token donor-badge uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-donor (err u102))
(define-constant err-already-verified (err u103))
(define-constant err-badge-not-found (err u104))

(define-data-var last-badge-id uint u0)
(define-data-var donation-threshold uint u1)

(define-map donor-records 
    principal 
    {
        donations: uint,
        last-donation: uint,
        verified: bool,
        badge-id: (optional uint)
    }
)

(define-map donation-centers principal bool)

(define-read-only (get-last-token-id)
    (ok (var-get last-badge-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok (some (concat "https://api.donorx.org/metadata/" (int-to-ascii  token-id))))
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? donor-badge token-id))
)

(define-read-only (get-donor-info (donor principal))
    (ok (map-get? donor-records donor))
)

(define-read-only (is-donation-center (center principal))
    (default-to false (map-get? donation-centers center))
)

(define-public (register-donation-center (center principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set donation-centers center true))
    )
)

(define-public (remove-donation-center (center principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-delete donation-centers center))
    )
)

(define-public (record-donation (donor principal))
    (let (
        (donor-data (unwrap! (map-get? donor-records donor) err-invalid-donor))
        (current-block stacks-block-height)
    )
        (asserts! (is-donation-center tx-sender) err-not-authorized)
        (asserts! (> current-block (+ (get last-donation donor-data) u8640)) err-already-verified)
        
        (map-set donor-records donor {
            donations: (+ (get donations donor-data) u1),
            last-donation: current-block,
            verified: (get verified donor-data),
            badge-id: (get badge-id donor-data)
        })
        
        (try! (check-and-mint-badge donor))
        (ok true)
    )
)

(define-public (register-donor)
    (begin
        (asserts! (is-none (map-get? donor-records tx-sender)) err-already-verified)
        (ok (map-set donor-records tx-sender {
            donations: u0,
            last-donation: u0,
            verified: true,
            badge-id: none
        }))
    )
)

(define-private (check-and-mint-badge (donor principal))
    (let (
        (donor-data (unwrap! (map-get? donor-records donor) err-invalid-donor))
    )
        (if (and
            (>= (get donations donor-data) (var-get donation-threshold))
            (is-none (get badge-id donor-data))
        )
            (mint-badge donor)
            (ok true)
        )
    )
)

(define-private (mint-badge (recipient principal))
    (let (
        (new-id (+ (var-get last-badge-id) u1))
        (donor-data (unwrap! (map-get? donor-records recipient) err-invalid-donor))
    )
        (try! (nft-mint? donor-badge new-id recipient))
        (var-set last-badge-id new-id)
        (ok (map-set donor-records recipient
            (merge donor-data { badge-id: (some new-id) })
        ))
    )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-authorized)
        (nft-transfer? donor-badge token-id sender recipient)
    )
)

(define-public (set-donation-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set donation-threshold new-threshold))
    )
)