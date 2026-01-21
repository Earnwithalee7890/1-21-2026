;; Crowdfunding Platform - Clarity 4
;; Create and fund campaigns

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u5000))
(define-constant err-unauthorized (err u5001))
(define-constant err-campaign-ended (err u5002))
(define-constant err-goal-not-met (err u5003))

(define-data-var campaign-nonce uint u0)
(define-data-var platform-fee uint u300)

(define-map campaigns
    uint
    {
        creator: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
        goal: uint,
        raised: uint,
        deadline: uint,
        claimed: bool
    }
)

(define-map contributions
    {campaign-id: uint, contributor: principal}
    {amount: uint, contributed-at: uint}
)

(define-read-only (get-campaign (campaign-id uint))
    (map-get? campaigns campaign-id)
)

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
    (map-get? contributions {campaign-id: campaign-id, contributor: contributor})
)

(define-public (create-campaign
    (title (string-utf8 100))
    (description (string-utf8 500))
    (goal uint)
    (duration uint)
)
    (let (
        (campaign-id (var-get campaign-nonce))
    )
        (map-set campaigns campaign-id {
            creator: tx-sender,
            title: title,
            description: description,
            goal: goal,
            raised: u0,
            deadline: (+ block-height duration),
            claimed: false
        })
        (var-set campaign-nonce (+ campaign-id u1))
        (ok campaign-id)
    )
)

(define-public (contribute (campaign-id uint) (amount uint))
    (let (
        (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    )
        (asserts! (<= block-height (get deadline campaign)) err-campaign-ended)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set campaigns campaign-id
            (merge campaign {raised: (+ (get raised campaign) amount)})
        )
        (ok (map-set contributions {campaign-id: campaign-id, contributor: tx-sender}
            {amount: amount, contributed-at: block-height}
        ))
    )
)

(define-public (claim-funds (campaign-id uint))
    (let (
        (campaign (unwrap! (get-campaign campaign-id) err-not-found))
        (fee (/ (* (get raised campaign) (var-get platform-fee)) u10000))
        (payout (- (get raised campaign) fee))
    )
        (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
        (asserts! (> block-height (get deadline campaign)) err-campaign-ended)
        (asserts! (>= (get raised campaign) (get goal campaign)) err-goal-not-met)
        (asserts! (not (get claimed campaign)) err-unauthorized)
        (map-set campaigns campaign-id (merge campaign {claimed: true}))
        (try! (as-contract (stx-transfer? payout tx-sender (get creator campaign))))
        (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
        (ok payout)
    )
)

(define-public (refund (campaign-id uint))
    (let (
        (campaign (unwrap! (get-campaign campaign-id) err-not-found))
        (contribution (unwrap! (get-contribution campaign-id tx-sender) err-not-found))
    )
        (asserts! (> block-height (get deadline campaign)) err-campaign-ended)
        (asserts! (< (get raised campaign) (get goal campaign)) err-goal-not-met)
        (try! (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender)))
        (ok (get amount contribution))
    )
)
