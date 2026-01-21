;; Simple Lock
;; Lock and unlock access

(define-data-var locked bool false)
(define-data-var lock-owner principal tx-sender)

(define-read-only (is-locked)
  (var-get locked)
)

(define-read-only (get-lock-owner)
  (var-get lock-owner)
)

(define-public (lock)
  (begin
    (asserts! (is-eq tx-sender (var-get lock-owner)) (err u100))
    (var-set locked true)
    (ok true)
  )
)

(define-public (unlock)
  (begin
    (asserts! (is-eq tx-sender (var-get lock-owner)) (err u100))
    (var-set locked false)
    (ok true)
  )
)

(define-public (transfer-lock (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get lock-owner)) (err u100))
    (var-set lock-owner new-owner)
    (ok true)
  )
)
