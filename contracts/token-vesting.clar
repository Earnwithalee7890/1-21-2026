;; Token Vesting Contract
;; Time-locked token distribution with cliff and vesting schedule

(define-map vesting-schedules
  { beneficiary: principal, schedule-id: uint }
  {
    token: principal,
    total-amount: uint,
    released-amount: uint,
    start-block: uint,
    cliff-duration: uint,
    vesting-duration: uint,
    revocable: bool,
    revoked: bool
  }
)

(define-map beneficiary-count
  { beneficiary: principal }
  { count: uint }
)

(define-data-var contract-owner principal tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-SCHEDULE-NOT-FOUND (err u101))
(define-constant ERR-CLIFF-NOT-REACHED (err u102))
(define-constant ERR-NOTHING-TO-RELEASE (err u103))
(define-constant ERR-SCHEDULE-REVOKED (err u104))
(define-constant ERR-NOT-REVOCABLE (err u105))

(define-read-only (get-schedule (beneficiary principal) (schedule-id uint))
  (map-get? vesting-schedules { beneficiary: beneficiary, schedule-id: schedule-id })
)

(define-read-only (get-beneficiary-schedule-count (beneficiary principal))
  (default-to { count: u0 }
    (map-get? beneficiary-count { beneficiary: beneficiary }))
)

(define-read-only (calculate-vested-amount (beneficiary principal) (schedule-id uint))
  (let (
    (schedule (unwrap-panic (get-schedule beneficiary schedule-id)))
    (elapsed (- block-height (get start-block schedule)))
    (cliff-end (+ (get start-block schedule) (get cliff-duration schedule)))
    (vesting-end (+ (get start-block schedule) (get vesting-duration schedule)))
  )
    (if (< block-height cliff-end)
      u0
      (if (>= block-height vesting-end)
        (get total-amount schedule)
        (/ (* (get total-amount schedule) (- block-height (get start-block schedule))) (get vesting-duration schedule))
      )
    )
  )
)

(define-read-only (calculate-releasable-amount (beneficiary principal) (schedule-id uint))
  (let (
    (schedule (unwrap-panic (get-schedule beneficiary schedule-id)))
    (vested (calculate-vested-amount beneficiary schedule-id))
  )
    (- vested (get released-amount schedule))
  )
)

(define-public (create-vesting-schedule 
  (beneficiary principal) 
  (token principal) 
  (total-amount uint) 
  (cliff-duration uint) 
  (vesting-duration uint)
  (revocable bool))
  (let (
    (current-count (get count (get-beneficiary-schedule-count beneficiary)))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set vesting-schedules
      { beneficiary: beneficiary, schedule-id: current-count }
      {
        token: token,
        total-amount: total-amount,
        released-amount: u0,
        start-block: block-height,
        cliff-duration: cliff-duration,
        vesting-duration: vesting-duration,
        revocable: revocable,
        revoked: false
      }
    )
    (map-set beneficiary-count
      { beneficiary: beneficiary }
      { count: (+ current-count u1) }
    )
    (ok current-count)
  )
)

(define-public (release (schedule-id uint))
  (let (
    (schedule (unwrap! (get-schedule tx-sender schedule-id) ERR-SCHEDULE-NOT-FOUND))
    (releasable (calculate-releasable-amount tx-sender schedule-id))
  )
    (asserts! (not (get revoked schedule)) ERR-SCHEDULE-REVOKED)
    (asserts! (> releasable u0) ERR-NOTHING-TO-RELEASE)
    (map-set vesting-schedules
      { beneficiary: tx-sender, schedule-id: schedule-id }
      (merge schedule {
        released-amount: (+ (get released-amount schedule) releasable)
      })
    )
    (ok releasable)
  )
)

(define-public (revoke (beneficiary principal) (schedule-id uint))
  (let (
    (schedule (unwrap! (get-schedule beneficiary schedule-id) ERR-SCHEDULE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (get revocable schedule) ERR-NOT-REVOCABLE)
    (map-set vesting-schedules
      { beneficiary: beneficiary, schedule-id: schedule-id }
      (merge schedule { revoked: true })
    )
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)
