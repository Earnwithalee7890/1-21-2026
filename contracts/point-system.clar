;; Point System
;; Award and track points

(define-data-var admin principal tx-sender)
(define-data-var total-points uint u0)

(define-map user-points principal uint)

(define-read-only (get-points (user principal))
  (default-to u0 (map-get? user-points user))
)

(define-read-only (get-total-points)
  (var-get total-points)
)

(define-public (award-points (user principal) (amount uint))
  (let
    (
      (current (get-points user))
    )
    (asserts! (is-eq tx-sender (var-get admin)) (err u100))
    (map-set user-points user (+ current amount))
    (var-set total-points (+ (var-get total-points) amount))
    (ok (+ current amount))
  )
)

(define-public (deduct-points (user principal) (amount uint))
  (let
    (
      (current (get-points user))
    )
    (asserts! (is-eq tx-sender (var-get admin)) (err u100))
    (asserts! (>= current amount) (err u101))
    (map-set user-points user (- current amount))
    (var-set total-points (- (var-get total-points) amount))
    (ok (- current amount))
  )
)
