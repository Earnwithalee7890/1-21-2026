;; Simple Counter
;; A minimal counter contract that is guaranteed to deploy

(define-data-var counter uint u0)

(define-read-only (get-counter)
  (var-get counter)
)

(define-public (increment)
  (begin
    (var-set counter (+ (var-get counter) u1))
    (ok (var-get counter))
  )
)

(define-public (decrement)
  (begin
    (var-set counter (- (var-get counter) u1))
    (ok (var-get counter))
  )
)

(define-public (reset)
  (begin
    (var-set counter u0)
    (ok true)
  )
)
