;; Simple Counter - Clarity 3
;; Basic counter contract

(define-data-var counter uint u0)
(define-data-var owner principal tx-sender)

(define-read-only (get-counter)
    (var-get counter)
)

(define-read-only (get-owner)
    (var-get owner)
)

(define-public (increment)
    (begin
        (var-set counter (+ (var-get counter) u1))
        (ok (var-get counter))
    )
)

(define-public (decrement)
    (let (
        (current (var-get counter))
    )
        (asserts! (> current u0) (err u100))
        (var-set counter (- current u1))
        (ok (var-get counter))
    )
)

(define-public (reset)
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) (err u101))
        (var-set counter u0)
        (ok true)
    )
)

(define-public (set-counter (value uint))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) (err u101))
        (var-set counter value)
        (ok value)
    )
)
