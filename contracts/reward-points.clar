;; Reward Points - Clarity 4
;; Loyalty points system

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1500))
(define-constant err-unauthorized (err u1501))
(define-constant err-insufficient-points (err u1502))

(define-data-var total-points-issued uint u0)
(define-data-var points-per-stx uint u100)

(define-map point-balances
    principal
    {balance: uint, lifetime-earned: uint, lifetime-spent: uint}
)

(define-map redemptions
    {user: principal, id: uint}
    {points-spent: uint, redeemed-at: uint, item: (string-utf8 100)}
)

(define-data-var redemption-nonce uint u0)

(define-read-only (get-balance (user principal))
    (default-to {balance: u0, lifetime-earned: u0, lifetime-spent: u0}
        (map-get? point-balances user)
    )
)

(define-read-only (get-points (user principal))
    (get balance (get-balance user))
)

(define-public (earn-points (user principal) (stx-spent uint))
    (let (
        (current (get-balance user))
        (points-to-add (* stx-spent (var-get points-per-stx)))
    )
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (var-set total-points-issued (+ (var-get total-points-issued) points-to-add))
        (ok (map-set point-balances user {
            balance: (+ (get balance current) points-to-add),
            lifetime-earned: (+ (get lifetime-earned current) points-to-add),
            lifetime-spent: (get lifetime-spent current)
        }))
    )
)

(define-public (award-points (user principal) (points uint))
    (let (
        (current (get-balance user))
    )
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (var-set total-points-issued (+ (var-get total-points-issued) points))
        (ok (map-set point-balances user {
            balance: (+ (get balance current) points),
            lifetime-earned: (+ (get lifetime-earned current) points),
            lifetime-spent: (get lifetime-spent current)
        }))
    )
)

(define-public (redeem-points (points uint) (item (string-utf8 100)))
    (let (
        (current (get-balance tx-sender))
        (rid (var-get redemption-nonce))
    )
        (asserts! (>= (get balance current) points) err-insufficient-points)
        (map-set point-balances tx-sender {
            balance: (- (get balance current) points),
            lifetime-earned: (get lifetime-earned current),
            lifetime-spent: (+ (get lifetime-spent current) points)
        })
        (map-set redemptions {user: tx-sender, id: rid}
            {points-spent: points, redeemed-at: stacks-block-height, item: item}
        )
        (var-set redemption-nonce (+ rid u1))
        (ok rid)
    )
)
