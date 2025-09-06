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
(define-constant err-campaign-not-found (err u105))
(define-constant err-campaign-ended (err u106))
(define-constant err-campaign-not-ended (err u107))
(define-constant err-insufficient-target (err u108))
(define-constant err-already-participated (err u109))
(define-constant err-campaign-not-started (err u110))
(define-constant err-invalid-endorsement (err u111))
(define-constant err-self-endorsement (err u112))
(define-constant err-already-endorsed (err u113))
(define-constant err-endorsement-not-found (err u114))
(define-constant err-invalid-reputation-tier (err u115))

(define-data-var last-badge-id uint u0)
(define-data-var donation-threshold uint u1)
(define-data-var last-campaign-id uint u0)
(define-data-var endorsement-weight uint u10)
(define-data-var reputation-decay-period uint u52560)

(define-map donor-records 
    principal 
    {
        donations: uint,
        last-donation: uint,
        verified: bool,
        badge-id: (optional uint),
        reputation-score: uint,
        endorsement-count: uint,
        last-reputation-update: uint
    }
)

(define-map donation-centers principal bool)

(define-map campaigns
    uint
    {
        creator: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        target-amount: uint,
        current-amount: uint,
        start-block: uint,
        end-block: uint,
        reward-per-donation: uint,
        max-participants: uint,
        participant-count: uint,
        active: bool
    }
)

(define-map campaign-participants
    { campaign-id: uint, participant: principal }
    { donation-amount: uint, timestamp: uint }
)

(define-map donor-endorsements
    { endorser: principal, endorsed: principal }
    { 
        endorsement-type: uint,
        timestamp: uint,
        message: (string-ascii 200),
        weight: uint
    }
)

(define-map reputation-tiers
    uint
    {
        min-score: uint,
        max-score: uint,
        tier-name: (string-ascii 50),
        benefits-multiplier: uint
    }
)

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

(define-read-only (get-campaign (campaign-id uint))
    (ok (map-get? campaigns campaign-id))
)

(define-read-only (get-campaign-participation (campaign-id uint) (participant principal))
    (ok (map-get? campaign-participants { campaign-id: campaign-id, participant: participant }))
)

(define-read-only (get-active-campaigns)
    (ok (var-get last-campaign-id))
)

(define-read-only (is-campaign-active (campaign-id uint))
    (let (
        (campaign-data (unwrap! (map-get? campaigns campaign-id) false))
        (current-block stacks-block-height)
    )
        (and
            (get active campaign-data)
            (>= current-block (get start-block campaign-data))
            (<= current-block (get end-block campaign-data))
        )
    )
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
        (donor-data (default-to { donations: u0, last-donation: u0, verified: false, badge-id: none, reputation-score: u0, endorsement-count: u0, last-reputation-update: u0 } 
                                (map-get? donor-records donor)))
        (current-block stacks-block-height)
        (new-reputation (+ (get reputation-score donor-data) u5))
    )
        (asserts! (is-donation-center tx-sender) err-not-authorized)
        (asserts! (> current-block (+ (get last-donation donor-data) u8640)) err-already-verified)
        
        (map-set donor-records donor {
            donations: (+ (get donations donor-data) u1),
            last-donation: current-block,
            verified: (get verified donor-data),
            badge-id: (get badge-id donor-data),
            reputation-score: new-reputation,
            endorsement-count: (get endorsement-count donor-data),
            last-reputation-update: current-block
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
            badge-id: none,
            reputation-score: u10,
            endorsement-count: u0,
            last-reputation-update: stacks-block-height
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

(define-public (create-campaign 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (target-amount uint)
    (duration-blocks uint)
    (reward-per-donation uint)
    (max-participants uint)
)
    (let (
        (new-campaign-id (+ (var-get last-campaign-id) u1))
        (current-block stacks-block-height)
        (end-block (+ current-block duration-blocks))
    )
        (asserts! (> target-amount u0) err-insufficient-target)
        (asserts! (> duration-blocks u0) err-insufficient-target)
        (asserts! (> max-participants u0) err-insufficient-target)
        
        (map-set campaigns new-campaign-id {
            creator: tx-sender,
            title: title,
            description: description,
            target-amount: target-amount,
            current-amount: u0,
            start-block: current-block,
            end-block: end-block,
            reward-per-donation: reward-per-donation,
            max-participants: max-participants,
            participant-count: u0,
            active: true
        })
        
        (var-set last-campaign-id new-campaign-id)
        (ok new-campaign-id)
    )
)

(define-public (participate-in-campaign (campaign-id uint) (donation-amount uint))
    (let (
        (campaign-data (unwrap! (map-get? campaigns campaign-id) err-campaign-not-found))
        (current-block stacks-block-height)
        (participant-key { campaign-id: campaign-id, participant: tx-sender })
        (existing-participation (map-get? campaign-participants participant-key))
    )
        (asserts! (is-campaign-active campaign-id) err-campaign-not-started)
        (asserts! (is-none existing-participation) err-already-participated)
        (asserts! (< (get participant-count campaign-data) (get max-participants campaign-data)) err-insufficient-target)
        (asserts! (> donation-amount u0) err-insufficient-target)
        
        (map-set campaign-participants participant-key {
            donation-amount: donation-amount,
            timestamp: current-block
        })
        
        (map-set campaigns campaign-id
            (merge campaign-data {
                current-amount: (+ (get current-amount campaign-data) donation-amount),
                participant-count: (+ (get participant-count campaign-data) u1)
            })
        )
        
        (ok true)
    )
)

(define-public (end-campaign (campaign-id uint))
    (let (
        (campaign-data (unwrap! (map-get? campaigns campaign-id) err-campaign-not-found))
        (current-block stacks-block-height)
    )
        (asserts! (is-eq tx-sender (get creator campaign-data)) err-not-authorized)
        (asserts! (> current-block (get end-block campaign-data)) err-campaign-not-ended)
        (asserts! (get active campaign-data) err-campaign-ended)
        
        (map-set campaigns campaign-id
            (merge campaign-data { active: false })
        )
        
        (ok true)
    )
)

(define-public (claim-campaign-reward (campaign-id uint))
    (let (
        (campaign-data (unwrap! (map-get? campaigns campaign-id) err-campaign-not-found))
        (participant-key { campaign-id: campaign-id, participant: tx-sender })
        (participation-data (unwrap! (map-get? campaign-participants participant-key) err-not-authorized))
        (current-block stacks-block-height)
        (reward-amount (get reward-per-donation campaign-data))
    )
        (asserts! (not (get active campaign-data)) err-campaign-not-ended)
        (asserts! (>= (get current-amount campaign-data) (get target-amount campaign-data)) err-insufficient-target)
        
        (map-delete campaign-participants participant-key)
        
        (ok reward-amount)
    )
)

(define-private (calculate-campaign-success-rate (campaign-id uint))
    (let (
        (campaign-data (unwrap! (map-get? campaigns campaign-id) u0))
        (target (get target-amount campaign-data))
        (current (get current-amount campaign-data))
    )
        (if (> target u0)
            (/ (* current u100) target)
            u0
        )
    )
)

(define-public (get-campaign-stats (campaign-id uint))
    (let (
        (campaign-data (unwrap! (map-get? campaigns campaign-id) err-campaign-not-found))
        (success-rate (calculate-campaign-success-rate campaign-id))
        (current-block stacks-block-height)
        (time-remaining (if (<= current-block (get end-block campaign-data)) 
                            (- (get end-block campaign-data) current-block) 
                            u0))
    )
        (ok {
            campaign: campaign-data,
            success-rate: success-rate,
            time-remaining: time-remaining,
            is-active: (is-campaign-active campaign-id)
        })
    )
)

(define-public (endorse-donor (endorsed principal) (endorsement-type uint) (message (string-ascii 200)))
    (let (
        (endorsement-key { endorser: tx-sender, endorsed: endorsed })
        (existing-endorsement (map-get? donor-endorsements endorsement-key))
        (endorsed-data (unwrap! (map-get? donor-records endorsed) err-invalid-donor))
        (current-block stacks-block-height)
        (endorsement-weight-value (var-get endorsement-weight))
        (new-reputation (+ (get reputation-score endorsed-data) endorsement-weight-value))
    )
        (asserts! (is-donation-center tx-sender) err-not-authorized)
        (asserts! (not (is-eq tx-sender endorsed)) err-self-endorsement)
        (asserts! (is-none existing-endorsement) err-already-endorsed)
        (asserts! (and (>= endorsement-type u1) (<= endorsement-type u5)) err-invalid-endorsement)
        
        (map-set donor-endorsements endorsement-key {
            endorsement-type: endorsement-type,
            timestamp: current-block,
            message: message,
            weight: endorsement-weight-value
        })
        
        (map-set donor-records endorsed
            (merge endorsed-data {
                reputation-score: new-reputation,
                endorsement-count: (+ (get endorsement-count endorsed-data) u1),
                last-reputation-update: current-block
            })
        )
        
        (ok true)
    )
)

(define-public (revoke-endorsement (endorsed principal))
    (let (
        (endorsement-key { endorser: tx-sender, endorsed: endorsed })
        (existing-endorsement (unwrap! (map-get? donor-endorsements endorsement-key) err-endorsement-not-found))
        (endorsed-data (unwrap! (map-get? donor-records endorsed) err-invalid-donor))
        (current-block stacks-block-height)
        (endorsement-weight-value (get weight existing-endorsement))
        (current-reputation (get reputation-score endorsed-data))
        (new-reputation (if (>= current-reputation endorsement-weight-value)
                           (- current-reputation endorsement-weight-value)
                           u0))
    )
        (asserts! (is-donation-center tx-sender) err-not-authorized)
        
        (map-delete donor-endorsements endorsement-key)
        
        (map-set donor-records endorsed
            (merge endorsed-data {
                reputation-score: new-reputation,
                endorsement-count: (if (> (get endorsement-count endorsed-data) u0)
                                     (- (get endorsement-count endorsed-data) u1)
                                     u0),
                last-reputation-update: current-block
            })
        )
        
        (ok true)
    )
)

(define-public (initialize-reputation-tiers)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set reputation-tiers u1 {
            min-score: u0,
            max-score: u49,
            tier-name: "Bronze Donor",
            benefits-multiplier: u100
        })
        
        (map-set reputation-tiers u2 {
            min-score: u50,
            max-score: u149,
            tier-name: "Silver Donor",
            benefits-multiplier: u150
        })
        
        (map-set reputation-tiers u3 {
            min-score: u150,
            max-score: u299,
            tier-name: "Gold Donor",
            benefits-multiplier: u200
        })
        
        (map-set reputation-tiers u4 {
            min-score: u300,
            max-score: u499,
            tier-name: "Platinum Donor",
            benefits-multiplier: u250
        })
        
        (map-set reputation-tiers u5 {
            min-score: u500,
            max-score: u999999,
            tier-name: "Diamond Donor",
            benefits-multiplier: u300
        })
        
        (ok true)
    )
)

(define-read-only (get-donor-reputation-tier (donor principal))
    (let (
        (donor-data (unwrap! (map-get? donor-records donor) err-invalid-donor))
        (reputation-score (get reputation-score donor-data))
    )
        (if (<= reputation-score u49)
            (ok (map-get? reputation-tiers u1))
            (if (<= reputation-score u149)
                (ok (map-get? reputation-tiers u2))
                (if (<= reputation-score u299)
                    (ok (map-get? reputation-tiers u3))
                    (if (<= reputation-score u499)
                        (ok (map-get? reputation-tiers u4))
                        (ok (map-get? reputation-tiers u5))
                    )
                )
            )
        )
    )
)

(define-read-only (get-donor-endorsements (endorsed principal))
    (let (
        (donor-data (map-get? donor-records endorsed))
    )
        (ok {
            donor-data: donor-data,
            endorsement-count: (match donor-data
                                 some-data (get endorsement-count some-data)
                                 u0)
        })
    )
)

(define-read-only (get-endorsement-details (endorser principal) (endorsed principal))
    (ok (map-get? donor-endorsements { endorser: endorser, endorsed: endorsed }))
)

(define-private (calculate-reputation-decay (donor principal))
    (let (
        (donor-data (unwrap! (map-get? donor-records donor) u0))
        (current-block stacks-block-height)
        (last-update (get last-reputation-update donor-data))
        (blocks-passed (- current-block last-update))
        (decay-period (var-get reputation-decay-period))
        (current-reputation (get reputation-score donor-data))
    )
        (if (>= blocks-passed decay-period)
            (let (
                (decay-amount (/ current-reputation u10))
                (new-reputation (if (>= current-reputation decay-amount)
                                  (- current-reputation decay-amount)
                                  u0))
            )
                new-reputation
            )
            current-reputation
        )
    )
)

(define-public (update-reputation-decay (donor principal))
    (let (
        (donor-data (unwrap! (map-get? donor-records donor) err-invalid-donor))
        (new-reputation (calculate-reputation-decay donor))
        (current-block stacks-block-height)
    )
        (map-set donor-records donor
            (merge donor-data {
                reputation-score: new-reputation,
                last-reputation-update: current-block
            })
        )
        (ok new-reputation)
    )
)

(define-public (set-endorsement-weight (new-weight uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set endorsement-weight new-weight))
    )
)

(define-public (set-reputation-decay-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set reputation-decay-period new-period))
    )
)


