;; Config Store
;; Store configuration values

(define-data-var admin principal tx-sender)

(define-map config-values (string-ascii 50) uint)
(define-map config-strings (string-ascii 50) (string-ascii 100))

(define-read-only (get-value (key (string-ascii 50)))
  (map-get? config-values key)
)

(define-read-only (get-string (key (string-ascii 50)))
  (map-get? config-strings key)
)

(define-public (set-value (key (string-ascii 50)) (value uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u100))
    (map-set config-values key value)
    (ok true)
  )
)

(define-public (set-string (key (string-ascii 50)) (value (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u100))
    (map-set config-strings key value)
    (ok true)
  )
)

(define-public (remove-value (key (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u100))
    (map-delete config-values key)
    (ok true)
  )
)
