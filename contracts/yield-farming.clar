;; Yield Farming Protocol
;; Stake tokens to earn rewards over time

(define-map staking-pools
  { pool-id: uint }
  {
    stake-token: principal,
    reward-token: principal,
    total-staked: uint,
    reward-rate: uint,
    last-update-block: uint,
    reward-per-token-stored: uint
  }
)

(define-map user-stakes
  { pool-id: uint, staker: principal }
  {
    amount: uint,
    reward-debt: uint,
    pending-rewards: uint
  }
)

(define-data-var pool-count uint u0)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POOL-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-ZERO-AMOUNT (err u103))

(define-read-only (get-pool-info (pool-id uint))
  (map-get? staking-pools { pool-id: pool-id })
)

(define-read-only (get-user-stake (pool-id uint) (user principal))
  (default-to { amount: u0, reward-debt: u0, pending-rewards: u0 }
    (map-get? user-stakes { pool-id: pool-id, staker: user }))
)

(define-read-only (calculate-pending-reward (pool-id uint) (user principal))
  (let (
    (pool (unwrap-panic (get-pool-info pool-id)))
    (stake (get-user-stake pool-id user))
    (reward-per-token (get reward-per-token-stored pool))
  )
    (+ (get pending-rewards stake)
       (/ (* (get amount stake) (- reward-per-token (get reward-debt stake))) u1000000))
  )
)

(define-public (create-pool (stake-token principal) (reward-token principal) (reward-rate uint))
  (let (
    (pool-id (var-get pool-count))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set staking-pools
      { pool-id: pool-id }
      {
        stake-token: stake-token,
        reward-token: reward-token,
        total-staked: u0,
        reward-rate: reward-rate,
        last-update-block: block-height,
        reward-per-token-stored: u0
      }
    )
    (var-set pool-count (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (stake (pool-id uint) (amount uint))
  (let (
    (pool (unwrap! (get-pool-info pool-id) ERR-POOL-NOT-FOUND))
    (user-info (get-user-stake pool-id tx-sender))
    (pending (calculate-pending-reward pool-id tx-sender))
  )
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (map-set staking-pools
      { pool-id: pool-id }
      (merge pool { total-staked: (+ (get total-staked pool) amount) })
    )
    (map-set user-stakes
      { pool-id: pool-id, staker: tx-sender }
      {
        amount: (+ (get amount user-info) amount),
        reward-debt: (get reward-per-token-stored pool),
        pending-rewards: pending
      }
    )
    (ok true)
  )
)

(define-public (unstake (pool-id uint) (amount uint))
  (let (
    (pool (unwrap! (get-pool-info pool-id) ERR-POOL-NOT-FOUND))
    (user-info (get-user-stake pool-id tx-sender))
    (pending (calculate-pending-reward pool-id tx-sender))
  )
    (asserts! (>= (get amount user-info) amount) ERR-INSUFFICIENT-BALANCE)
    (map-set staking-pools
      { pool-id: pool-id }
      (merge pool { total-staked: (- (get total-staked pool) amount) })
    )
    (map-set user-stakes
      { pool-id: pool-id, staker: tx-sender }
      {
        amount: (- (get amount user-info) amount),
        reward-debt: (get reward-per-token-stored pool),
        pending-rewards: pending
      }
    )
    (ok true)
  )
)

(define-public (claim-rewards (pool-id uint))
  (let (
    (pending (calculate-pending-reward pool-id tx-sender))
    (pool (unwrap! (get-pool-info pool-id) ERR-POOL-NOT-FOUND))
    (user-info (get-user-stake pool-id tx-sender))
  )
    (map-set user-stakes
      { pool-id: pool-id, staker: tx-sender }
      (merge user-info {
        pending-rewards: u0,
        reward-debt: (get reward-per-token-stored pool)
      })
    )
    (ok pending)
  )
)
