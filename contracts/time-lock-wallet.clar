;; Time-Lock Wallet
;; Lock tokens until a specified time

(define-map time-locks
  { lock-id: uint }
  {
    owner: principal,
    beneficiary: principal,
    amount: uint,
    unlock-block: uint,
    token: principal,
    released: bool
  }
)

(define-map user-locks
  { user: principal }
  { lock-ids: (list 50 uint) }
)

(define-data-var lock-nonce uint u0)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LOCK-NOT-FOUND (err u101))
(define-constant ERR-NOT-YET-UNLOCKED (err u102))
(define-constant ERR-ALREADY-RELEASED (err u103))
(define-constant ERR-ZERO-AMOUNT (err u104))

(define-read-only (get-lock (lock-id uint))
  (map-get? time-locks { lock-id: lock-id })
)

(define-read-only (get-user-locks (user principal))
  (default-to { lock-ids: (list ) }
    (map-get? user-locks { user: user }))
)

(define-read-only (is-unlocked (lock-id uint))
  (let (
    (lock (unwrap-panic (get-lock lock-id)))
  )
    (>= block-height (get unlock-block lock))
  )
)

(define-read-only (get-time-until-unlock (lock-id uint))
  (let (
    (lock (unwrap-panic (get-lock lock-id)))
  )
    (if (>= block-height (get unlock-block lock))
      u0
      (- (get unlock-block lock) block-height)
    )
  )
)

(define-public (create-lock (beneficiary principal) (amount uint) (unlock-blocks uint) (token principal))
  (let (
    (lock-id (var-get lock-nonce))
    (user-lock-list (get lock-ids (get-user-locks tx-sender)))
  )
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (map-set time-locks
      { lock-id: lock-id }
      {
        owner: tx-sender,
        beneficiary: beneficiary,
        amount: amount,
        unlock-block: (+ block-height unlock-blocks),
        token: token,
        released: false
      }
    )
    (var-set lock-nonce (+ lock-id u1))
    (ok lock-id)
  )
)

(define-public (release (lock-id uint))
  (let (
    (lock (unwrap! (get-lock lock-id) ERR-LOCK-NOT-FOUND))
  )
    (asserts! (or (is-eq tx-sender (get owner lock)) (is-eq tx-sender (get beneficiary lock))) ERR-NOT-AUTHORIZED)
    (asserts! (is-unlocked lock-id) ERR-NOT-YET-UNLOCKED)
    (asserts! (not (get released lock)) ERR-ALREADY-RELEASED)
    (map-set time-locks
      { lock-id: lock-id }
      (merge lock { released: true })
    )
    (ok (get amount lock))
  )
)

(define-public (extend-lock (lock-id uint) (additional-blocks uint))
  (let (
    (lock (unwrap! (get-lock lock-id) ERR-LOCK-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner lock)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get released lock)) ERR-ALREADY-RELEASED)
    (map-set time-locks
      { lock-id: lock-id }
      (merge lock { unlock-block: (+ (get unlock-block lock) additional-blocks) })
    )
    (ok (+ (get unlock-block lock) additional-blocks))
  )
)

(define-public (update-beneficiary (lock-id uint) (new-beneficiary principal))
  (let (
    (lock (unwrap! (get-lock lock-id) ERR-LOCK-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner lock)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get released lock)) ERR-ALREADY-RELEASED)
    (map-set time-locks
      { lock-id: lock-id }
      (merge lock { beneficiary: new-beneficiary })
    )
    (ok true)
  )
)
