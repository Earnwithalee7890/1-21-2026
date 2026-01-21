;; Collateralized Lending Protocol
;; Deposit collateral to borrow assets

(define-map collateral-positions
  { user: principal }
  {
    collateral-amount: uint,
    borrowed-amount: uint,
    collateral-token: principal,
    borrow-token: principal,
    last-interest-block: uint
  }
)

(define-map market-config
  { token: principal }
  {
    collateral-factor: uint,
    liquidation-threshold: uint,
    interest-rate: uint,
    total-supply: uint,
    total-borrowed: uint
  }
)

(define-data-var liquidation-bonus uint u500)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-MARKET-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u102))
(define-constant ERR-POSITION-NOT-FOUND (err u103))
(define-constant ERR-HEALTHY-POSITION (err u104))
(define-constant ERR-ZERO-AMOUNT (err u105))

(define-read-only (get-position (user principal))
  (map-get? collateral-positions { user: user })
)

(define-read-only (get-market (token principal))
  (map-get? market-config { token: token })
)

(define-read-only (calculate-health-factor (user principal))
  (let (
    (position (unwrap-panic (get-position user)))
    (collateral-value (get collateral-amount position))
    (borrowed-value (get borrowed-amount position))
  )
    (if (is-eq borrowed-value u0)
      u999999
      (/ (* collateral-value u10000) borrowed-value)
    )
  )
)

(define-read-only (get-max-borrow (collateral-amount uint) (collateral-factor uint))
  (/ (* collateral-amount collateral-factor) u10000)
)

(define-public (create-market (token principal) (collateral-factor uint) (liquidation-threshold uint) (interest-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set market-config
      { token: token }
      {
        collateral-factor: collateral-factor,
        liquidation-threshold: liquidation-threshold,
        interest-rate: interest-rate,
        total-supply: u0,
        total-borrowed: u0
      }
    )
    (ok true)
  )
)

(define-public (deposit-collateral (token principal) (amount uint))
  (let (
    (existing-position (get-position tx-sender))
    (market (unwrap! (get-market token) ERR-MARKET-NOT-FOUND))
  )
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (map-set collateral-positions
      { user: tx-sender }
      {
        collateral-amount: (+ (default-to u0 (get collateral-amount existing-position)) amount),
        borrowed-amount: (default-to u0 (get borrowed-amount existing-position)),
        collateral-token: token,
        borrow-token: (default-to token (get borrow-token existing-position)),
        last-interest-block: block-height
      }
    )
    (map-set market-config
      { token: token }
      (merge market { total-supply: (+ (get total-supply market) amount) })
    )
    (ok true)
  )
)

(define-public (borrow (token principal) (amount uint))
  (let (
    (position (unwrap! (get-position tx-sender) ERR-POSITION-NOT-FOUND))
    (market (unwrap! (get-market (get collateral-token position)) ERR-MARKET-NOT-FOUND))
    (max-borrow (get-max-borrow (get collateral-amount position) (get collateral-factor market)))
  )
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (asserts! (<= (+ (get borrowed-amount position) amount) max-borrow) ERR-INSUFFICIENT-COLLATERAL)
    (map-set collateral-positions
      { user: tx-sender }
      (merge position {
        borrowed-amount: (+ (get borrowed-amount position) amount),
        borrow-token: token
      })
    )
    (ok true)
  )
)

(define-public (repay (amount uint))
  (let (
    (position (unwrap! (get-position tx-sender) ERR-POSITION-NOT-FOUND))
  )
    (map-set collateral-positions
      { user: tx-sender }
      (merge position {
        borrowed-amount: (- (get borrowed-amount position) (min amount (get borrowed-amount position)))
      })
    )
    (ok true)
  )
)

(define-public (liquidate (user principal))
  (let (
    (position (unwrap! (get-position user) ERR-POSITION-NOT-FOUND))
    (health (calculate-health-factor user))
    (market (unwrap! (get-market (get collateral-token position)) ERR-MARKET-NOT-FOUND))
  )
    (asserts! (< health (get liquidation-threshold market)) ERR-HEALTHY-POSITION)
    (map-delete collateral-positions { user: user })
    (ok (get collateral-amount position))
  )
)
