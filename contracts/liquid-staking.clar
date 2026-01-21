;; Liquid Staking Token
;; Stake STX and receive liquid staking tokens

(define-fungible-token stSTX)

(define-map staking-pool
  { pool-id: uint }
  {
    total-staked: uint,
    total-stSTX: uint,
    exchange-rate: uint,
    rewards-accumulated: uint,
    last-update-block: uint,
    apy: uint
  }
)

(define-map user-stakes
  { user: principal }
  {
    stx-staked: uint,
    stSTX-balance: uint,
    staked-at: uint
  }
)

(define-data-var current-pool-id uint u0)
(define-data-var min-stake uint u1000000)
(define-data-var unstake-delay uint u2100)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-BELOW-MIN-STAKE (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-POOL-NOT-FOUND (err u103))

(define-read-only (get-pool-info)
  (map-get? staking-pool { pool-id: (var-get current-pool-id) })
)

(define-read-only (get-user-stake (user principal))
  (default-to { stx-staked: u0, stSTX-balance: u0, staked-at: u0 }
    (map-get? user-stakes { user: user }))
)

(define-read-only (get-exchange-rate)
  (match (get-pool-info)
    pool (get exchange-rate pool)
    u1000000
  )
)

(define-read-only (stx-to-stSTX (stx-amount uint))
  (/ (* stx-amount u1000000) (get-exchange-rate))
)

(define-read-only (stSTX-to-stx (stSTX-amount uint))
  (/ (* stSTX-amount (get-exchange-rate)) u1000000)
)

(define-read-only (get-total-value-locked)
  (match (get-pool-info)
    pool (get total-staked pool)
    u0
  )
)

(define-public (initialize-pool (initial-apy uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set staking-pool
      { pool-id: u0 }
      {
        total-staked: u0,
        total-stSTX: u0,
        exchange-rate: u1000000,
        rewards-accumulated: u0,
        last-update-block: block-height,
        apy: initial-apy
      }
    )
    (ok true)
  )
)

(define-public (stake (amount uint))
  (let (
    (pool (unwrap! (get-pool-info) ERR-POOL-NOT-FOUND))
    (user-info (get-user-stake tx-sender))
    (stSTX-amount (stx-to-stSTX amount))
  )
    (asserts! (>= amount (var-get min-stake)) ERR-BELOW-MIN-STAKE)
    (try! (ft-mint? stSTX stSTX-amount tx-sender))
    (map-set user-stakes
      { user: tx-sender }
      {
        stx-staked: (+ (get stx-staked user-info) amount),
        stSTX-balance: (+ (get stSTX-balance user-info) stSTX-amount),
        staked-at: block-height
      }
    )
    (map-set staking-pool
      { pool-id: (var-get current-pool-id) }
      (merge pool {
        total-staked: (+ (get total-staked pool) amount),
        total-stSTX: (+ (get total-stSTX pool) stSTX-amount)
      })
    )
    (ok stSTX-amount)
  )
)

(define-public (unstake (stSTX-amount uint))
  (let (
    (pool (unwrap! (get-pool-info) ERR-POOL-NOT-FOUND))
    (user-info (get-user-stake tx-sender))
    (stx-amount (stSTX-to-stx stSTX-amount))
  )
    (asserts! (>= (get stSTX-balance user-info) stSTX-amount) ERR-INSUFFICIENT-BALANCE)
    (try! (ft-burn? stSTX stSTX-amount tx-sender))
    (map-set user-stakes
      { user: tx-sender }
      (merge user-info {
        stx-staked: (- (get stx-staked user-info) stx-amount),
        stSTX-balance: (- (get stSTX-balance user-info) stSTX-amount)
      })
    )
    (map-set staking-pool
      { pool-id: (var-get current-pool-id) }
      (merge pool {
        total-staked: (- (get total-staked pool) stx-amount),
        total-stSTX: (- (get total-stSTX pool) stSTX-amount)
      })
    )
    (ok stx-amount)
  )
)

(define-public (compound-rewards (rewards-amount uint))
  (let (
    (pool (unwrap! (get-pool-info) ERR-POOL-NOT-FOUND))
    (new-rate (/ (* (+ (get total-staked pool) rewards-amount) u1000000) (get total-stSTX pool)))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set staking-pool
      { pool-id: (var-get current-pool-id) }
      (merge pool {
        total-staked: (+ (get total-staked pool) rewards-amount),
        exchange-rate: new-rate,
        rewards-accumulated: (+ (get rewards-accumulated pool) rewards-amount),
        last-update-block: block-height
      })
    )
    (ok new-rate)
  )
)

(define-public (update-apy (new-apy uint))
  (let (
    (pool (unwrap! (get-pool-info) ERR-POOL-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set staking-pool
      { pool-id: (var-get current-pool-id) }
      (merge pool { apy: new-apy })
    )
    (ok new-apy)
  )
)
