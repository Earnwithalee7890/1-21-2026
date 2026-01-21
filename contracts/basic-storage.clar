;; Basic Storage
;; Store and retrieve data

(define-data-var owner principal tx-sender)
(define-data-var stored-value uint u0)
(define-data-var stored-text (string-ascii 100) "")

(define-read-only (get-owner)
  (var-get owner)
)

(define-read-only (get-value)
  (var-get stored-value)
)

(define-read-only (get-text)
  (var-get stored-text)
)

(define-public (set-value (new-value uint))
  (begin
    (var-set stored-value new-value)
    (ok new-value)
  )
)

(define-public (set-text (new-text (string-ascii 100)))
  (begin
    (var-set stored-text new-text)
    (ok true)
  )
)
