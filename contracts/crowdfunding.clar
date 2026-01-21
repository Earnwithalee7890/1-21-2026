;; Crowdfunding Platform
;; Create and fund crowdfunding campaigns

(define-map campaigns
  { campaign-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    goal: uint,
    raised: uint,
    deadline: uint,
    status: (string-ascii 10),
    claimed: bool
  }
)

(define-map contributions
  { campaign-id: uint, contributor: principal }
  { amount: uint, refunded: bool }
)

(define-map campaign-contributors
  { campaign-id: uint }
  { count: uint }
)

(define-data-var campaign-nonce uint u0)
(define-data-var platform-fee uint u300)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-CAMPAIGN-NOT-FOUND (err u101))
(define-constant ERR-CAMPAIGN-ENDED (err u102))
(define-constant ERR-GOAL-NOT-REACHED (err u103))
(define-constant ERR-ALREADY-CLAIMED (err u104))
(define-constant ERR-NO-CONTRIBUTION (err u105))
(define-constant ERR-ALREADY-REFUNDED (err u106))

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (map-get? contributions { campaign-id: campaign-id, contributor: contributor })
)

(define-read-only (is-successful (campaign-id uint))
  (let (
    (campaign (unwrap-panic (get-campaign campaign-id)))
  )
    (>= (get raised campaign) (get goal campaign))
  )
)

(define-read-only (is-active (campaign-id uint))
  (let (
    (campaign (unwrap-panic (get-campaign campaign-id)))
  )
    (and (is-eq (get status campaign) "active")
         (<= block-height (get deadline campaign)))
  )
)

(define-public (create-campaign (title (string-ascii 100)) (description (string-ascii 500)) (goal uint) (duration-blocks uint))
  (let (
    (campaign-id (var-get campaign-nonce))
  )
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        goal: goal,
        raised: u0,
        deadline: (+ block-height duration-blocks),
        status: "active",
        claimed: false
      }
    )
    (map-set campaign-contributors
      { campaign-id: campaign-id }
      { count: u0 }
    )
    (var-set campaign-nonce (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (contribute (campaign-id uint) (amount uint))
  (let (
    (campaign (unwrap! (get-campaign campaign-id) ERR-CAMPAIGN-NOT-FOUND))
    (existing (get-contribution campaign-id tx-sender))
    (prev-amount (default-to u0 (get amount existing)))
    (contributors (default-to { count: u0 } (map-get? campaign-contributors { campaign-id: campaign-id })))
  )
    (asserts! (is-active campaign-id) ERR-CAMPAIGN-ENDED)
    (map-set contributions
      { campaign-id: campaign-id, contributor: tx-sender }
      { amount: (+ prev-amount amount), refunded: false }
    )
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { raised: (+ (get raised campaign) amount) })
    )
    (if (is-none existing)
      (map-set campaign-contributors
        { campaign-id: campaign-id }
        { count: (+ (get count contributors) u1) })
      true
    )
    (ok (+ (get raised campaign) amount))
  )
)

(define-public (claim-funds (campaign-id uint))
  (let (
    (campaign (unwrap! (get-campaign campaign-id) ERR-CAMPAIGN-NOT-FOUND))
    (fee (/ (* (get raised campaign) (var-get platform-fee)) u10000))
    (payout (- (get raised campaign) fee))
  )
    (asserts! (is-eq tx-sender (get creator campaign)) ERR-NOT-AUTHORIZED)
    (asserts! (is-successful campaign-id) ERR-GOAL-NOT-REACHED)
    (asserts! (not (get claimed campaign)) ERR-ALREADY-CLAIMED)
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { claimed: true, status: "completed" })
    )
    (ok payout)
  )
)

(define-public (request-refund (campaign-id uint))
  (let (
    (campaign (unwrap! (get-campaign campaign-id) ERR-CAMPAIGN-NOT-FOUND))
    (contribution (unwrap! (get-contribution campaign-id tx-sender) ERR-NO-CONTRIBUTION))
  )
    (asserts! (> block-height (get deadline campaign)) ERR-CAMPAIGN-ENDED)
    (asserts! (not (is-successful campaign-id)) ERR-GOAL-NOT-REACHED)
    (asserts! (not (get refunded contribution)) ERR-ALREADY-REFUNDED)
    (map-set contributions
      { campaign-id: campaign-id, contributor: tx-sender }
      (merge contribution { refunded: true })
    )
    (ok (get amount contribution))
  )
)
