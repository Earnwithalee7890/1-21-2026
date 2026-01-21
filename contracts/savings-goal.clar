;; Savings Goal - Clarity 4
;; Save STX toward goals

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1800))
(define-constant err-unauthorized (err u1801))
(define-constant err-goal-not-reached (err u1802))

(define-data-var goal-nonce uint u0)

(define-map savings-goals
    uint
    {
        owner: principal,
        name: (string-utf8 50),
        target: uint,
        saved: uint,
        deadline: uint,
        withdrawn: bool
    }
)

(define-read-only (get-goal (goal-id uint))
    (map-get? savings-goals goal-id)
)

(define-read-only (get-progress (goal-id uint))
    (match (get-goal goal-id)
        g (/ (* (get saved g) u100) (get target g))
        u0
    )
)

(define-public (create-goal
    (name (string-utf8 50))
    (target uint)
    (deadline uint)
)
    (let (
        (goal-id (var-get goal-nonce))
    )
        (map-set savings-goals goal-id {
            owner: tx-sender,
            name: name,
            target: target,
            saved: u0,
            deadline: (+ block-height deadline),
            withdrawn: false
        })
        (var-set goal-nonce (+ goal-id u1))
        (ok goal-id)
    )
)

(define-public (deposit-savings (goal-id uint) (amount uint))
    (let (
        (goal (unwrap! (get-goal goal-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get owner goal)) err-unauthorized)
        (asserts! (not (get withdrawn goal)) err-unauthorized)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (ok (map-set savings-goals goal-id
            (merge goal {saved: (+ (get saved goal) amount)})
        ))
    )
)

(define-public (withdraw-savings (goal-id uint))
    (let (
        (goal (unwrap! (get-goal goal-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get owner goal)) err-unauthorized)
        (asserts! (not (get withdrawn goal)) err-unauthorized)
        (asserts! (or (>= (get saved goal) (get target goal)) (> block-height (get deadline goal))) err-goal-not-reached)
        (map-set savings-goals goal-id (merge goal {withdrawn: true}))
        (try! (as-contract (stx-transfer? (get saved goal) tx-sender (get owner goal))))
        (ok (get saved goal))
    )
)
