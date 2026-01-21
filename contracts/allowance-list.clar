;; Allowance List
;; Manage an allowlist of principals

(define-data-var list-owner principal tx-sender)

(define-map allowed principal bool)

(define-read-only (is-allowed (addr principal))
  (default-to false (map-get? allowed addr))
)

(define-read-only (get-owner)
  (var-get list-owner)
)

(define-public (add-to-list (addr principal))
  (begin
    (asserts! (is-eq tx-sender (var-get list-owner)) (err u100))
    (map-set allowed addr true)
    (ok true)
  )
)

(define-public (remove-from-list (addr principal))
  (begin
    (asserts! (is-eq tx-sender (var-get list-owner)) (err u100))
    (map-set allowed addr false)
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get list-owner)) (err u100))
    (var-set list-owner new-owner)
    (ok true)
  )
)
