;; Price Feed
;; Store and retrieve price data

(define-data-var current-price uint u0)
(define-data-var last-update uint u0)
(define-data-var oracle principal tx-sender)

(define-read-only (get-price)
  (var-get current-price)
)

(define-read-only (get-last-update)
  (var-get last-update)
)

(define-read-only (get-oracle)
  (var-get oracle)
)

(define-public (set-price (price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle)) (err u100))
    (var-set current-price price)
    (var-set last-update block-height)
    (ok price)
  )
)

(define-public (set-oracle (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle)) (err u100))
    (var-set oracle new-oracle)
    (ok true)
  )
)
