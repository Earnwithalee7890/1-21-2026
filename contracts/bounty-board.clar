;; Bounty Board - Clarity 4
;; Create and claim bounties

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u7000))
(define-constant err-unauthorized (err u7001))
(define-constant err-already-claimed (err u7002))
(define-constant err-expired (err u7003))

(define-data-var bounty-nonce uint u0)

(define-map bounties
    uint
    {
        creator: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
        reward: uint,
        deadline: uint,
        claimed: bool,
        claimer: (optional principal)
    }
)

(define-map submissions
    {bounty-id: uint, submitter: principal}
    {solution: (string-utf8 300), submitted-at: uint, approved: bool}
)

(define-read-only (get-bounty (bounty-id uint))
    (map-get? bounties bounty-id)
)

(define-read-only (get-submission (bounty-id uint) (submitter principal))
    (map-get? submissions {bounty-id: bounty-id, submitter: submitter})
)

(define-public (create-bounty
    (title (string-utf8 100))
    (description (string-utf8 500))
    (reward uint)
    (duration uint)
)
    (let (
        (bounty-id (var-get bounty-nonce))
    )
        (try! (stx-transfer? reward tx-sender (as-contract tx-sender)))
        (map-set bounties bounty-id {
            creator: tx-sender,
            title: title,
            description: description,
            reward: reward,
            deadline: (+ block-height duration),
            claimed: false,
            claimer: none
        })
        (var-set bounty-nonce (+ bounty-id u1))
        (ok bounty-id)
    )
)

(define-public (submit-solution (bounty-id uint) (solution (string-utf8 300)))
    (let (
        (bounty (unwrap! (get-bounty bounty-id) err-not-found))
    )
        (asserts! (<= block-height (get deadline bounty)) err-expired)
        (asserts! (not (get claimed bounty)) err-already-claimed)
        (ok (map-set submissions {bounty-id: bounty-id, submitter: tx-sender}
            {solution: solution, submitted-at: block-height, approved: false}
        ))
    )
)

(define-public (approve-submission (bounty-id uint) (winner principal))
    (let (
        (bounty (unwrap! (get-bounty bounty-id) err-not-found))
        (submission (unwrap! (get-submission bounty-id winner) err-not-found))
    )
        (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
        (asserts! (not (get claimed bounty)) err-already-claimed)
        (map-set submissions {bounty-id: bounty-id, submitter: winner}
            (merge submission {approved: true})
        )
        (map-set bounties bounty-id
            (merge bounty {claimed: true, claimer: (some winner)})
        )
        (try! (as-contract (stx-transfer? (get reward bounty) tx-sender winner)))
        (ok true)
    )
)

(define-public (cancel-bounty (bounty-id uint))
    (let (
        (bounty (unwrap! (get-bounty bounty-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
        (asserts! (not (get claimed bounty)) err-already-claimed)
        (asserts! (> block-height (get deadline bounty)) err-expired)
        (try! (as-contract (stx-transfer? (get reward bounty) tx-sender (get creator bounty))))
        (ok true)
    )
)
