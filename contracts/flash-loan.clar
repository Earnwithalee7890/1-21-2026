;; Flash Loan Protocol
;; Borrow assets without collateral within single transaction

(define-map loan-pools
  { token: principal }
  {
    available-liquidity: uint,
    total-borrowed: uint,
    flash-fee: uint,
    protocol-fee: uint
  }
)

(define-map flash-loan-active
  { borrower: principal }
  { active: bool, token: principal, amount: uint }
)

(define-data-var protocol-treasury principal tx-sender)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POOL-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u102))
(define-constant ERR-LOAN-NOT-REPAID (err u103))
(define-constant ERR-LOAN-ALREADY-ACTIVE (err u104))

(define-read-only (get-pool (token principal))
  (map-get? loan-pools { token: token })
)

(define-read-only (get-flash-fee (token principal) (amount uint))
  (let (
    (pool (unwrap-panic (get-pool token)))
  )
    (/ (* amount (get flash-fee pool)) u10000)
  )
)

(define-read-only (is-loan-active (borrower principal))
  (default-to { active: false, token: tx-sender, amount: u0 }
    (map-get? flash-loan-active { borrower: borrower }))
)

(define-public (create-pool (token principal) (initial-liquidity uint) (flash-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set loan-pools
      { token: token }
      {
        available-liquidity: initial-liquidity,
        total-borrowed: u0,
        flash-fee: flash-fee,
        protocol-fee: u0
      }
    )
    (ok true)
  )
)

(define-public (deposit-liquidity (token principal) (amount uint))
  (let (
    (pool (unwrap! (get-pool token) ERR-POOL-NOT-FOUND))
  )
    (map-set loan-pools
      { token: token }
      (merge pool {
        available-liquidity: (+ (get available-liquidity pool) amount)
      })
    )
    (ok true)
  )
)

(define-public (borrow-flash-loan (token principal) (amount uint))
  (let (
    (pool (unwrap! (get-pool token) ERR-POOL-NOT-FOUND))
    (loan-status (is-loan-active tx-sender))
  )
    (asserts! (not (get active loan-status)) ERR-LOAN-ALREADY-ACTIVE)
    (asserts! (>= (get available-liquidity pool) amount) ERR-INSUFFICIENT-LIQUIDITY)
    (map-set flash-loan-active
      { borrower: tx-sender }
      { active: true, token: token, amount: amount }
    )
    (map-set loan-pools
      { token: token }
      (merge pool {
        available-liquidity: (- (get available-liquidity pool) amount),
        total-borrowed: (+ (get total-borrowed pool) amount)
      })
    )
    (ok amount)
  )
)

(define-public (repay-flash-loan (token principal) (amount uint))
  (let (
    (pool (unwrap! (get-pool token) ERR-POOL-NOT-FOUND))
    (loan-status (is-loan-active tx-sender))
    (fee (get-flash-fee token (get amount loan-status)))
    (total-repay (+ (get amount loan-status) fee))
  )
    (asserts! (get active loan-status) ERR-LOAN-NOT-REPAID)
    (asserts! (>= amount total-repay) ERR-LOAN-NOT-REPAID)
    (map-set flash-loan-active
      { borrower: tx-sender }
      { active: false, token: token, amount: u0 }
    )
    (map-set loan-pools
      { token: token }
      (merge pool {
        available-liquidity: (+ (get available-liquidity pool) total-repay),
        total-borrowed: (- (get total-borrowed pool) (get amount loan-status)),
        protocol-fee: (+ (get protocol-fee pool) fee)
      })
    )
    (ok true)
  )
)
