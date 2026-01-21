;; Token Swap DEX - Automated Market Maker
;; Enables trustless token swaps with liquidity pools

(define-fungible-token lp-token)

(define-map liquidity-pools
  { pool-id: uint }
  {
    token-a: principal,
    token-b: principal,
    reserve-a: uint,
    reserve-b: uint,
    total-shares: uint,
    fee-rate: uint
  }
)

(define-map user-liquidity
  { pool-id: uint, provider: principal }
  { shares: uint }
)

(define-data-var pool-nonce uint u0)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POOL-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u102))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u103))
(define-constant ERR-ZERO-AMOUNT (err u104))

(define-read-only (get-pool (pool-id uint))
  (map-get? liquidity-pools { pool-id: pool-id })
)

(define-read-only (get-user-shares (pool-id uint) (user principal))
  (default-to { shares: u0 }
    (map-get? user-liquidity { pool-id: pool-id, provider: user }))
)

(define-read-only (calculate-swap-output (input-amount uint) (input-reserve uint) (output-reserve uint) (fee uint))
  (let (
    (input-with-fee (* input-amount (- u10000 fee)))
    (numerator (* input-with-fee output-reserve))
    (denominator (+ (* input-reserve u10000) input-with-fee))
  )
    (/ numerator denominator)
  )
)

(define-public (create-pool (token-a principal) (token-b principal) (initial-a uint) (initial-b uint))
  (let (
    (pool-id (var-get pool-nonce))
  )
    (map-set liquidity-pools
      { pool-id: pool-id }
      {
        token-a: token-a,
        token-b: token-b,
        reserve-a: initial-a,
        reserve-b: initial-b,
        total-shares: initial-a,
        fee-rate: u30
      }
    )
    (map-set user-liquidity
      { pool-id: pool-id, provider: tx-sender }
      { shares: initial-a }
    )
    (var-set pool-nonce (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (add-liquidity (pool-id uint) (amount-a uint) (amount-b uint))
  (let (
    (pool (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (share-ratio (/ (* amount-a (get total-shares pool)) (get reserve-a pool)))
    (user-shares (get shares (get-user-shares pool-id tx-sender)))
  )
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool {
        reserve-a: (+ (get reserve-a pool) amount-a),
        reserve-b: (+ (get reserve-b pool) amount-b),
        total-shares: (+ (get total-shares pool) share-ratio)
      })
    )
    (map-set user-liquidity
      { pool-id: pool-id, provider: tx-sender }
      { shares: (+ user-shares share-ratio) }
    )
    (ok share-ratio)
  )
)

(define-public (swap (pool-id uint) (input-amount uint) (min-output uint) (is-a-to-b bool))
  (let (
    (pool (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (output-amount (if is-a-to-b
      (calculate-swap-output input-amount (get reserve-a pool) (get reserve-b pool) (get fee-rate pool))
      (calculate-swap-output input-amount (get reserve-b pool) (get reserve-a pool) (get fee-rate pool))
    ))
  )
    (asserts! (> input-amount u0) ERR-ZERO-AMOUNT)
    (asserts! (>= output-amount min-output) ERR-SLIPPAGE-TOO-HIGH)
    (map-set liquidity-pools
      { pool-id: pool-id }
      (if is-a-to-b
        (merge pool {
          reserve-a: (+ (get reserve-a pool) input-amount),
          reserve-b: (- (get reserve-b pool) output-amount)
        })
        (merge pool {
          reserve-a: (- (get reserve-a pool) output-amount),
          reserve-b: (+ (get reserve-b pool) input-amount)
        })
      )
    )
    (ok output-amount)
  )
)
