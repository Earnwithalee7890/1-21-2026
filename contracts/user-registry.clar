;; User Registry
;; Register usernames on-chain

(define-data-var registry-count uint u0)
(define-data-var admin principal tx-sender)

(define-map usernames principal (string-ascii 50))
(define-map name-taken (string-ascii 50) bool)

(define-read-only (get-username (user principal))
  (map-get? usernames user)
)

(define-read-only (is-name-taken (name (string-ascii 50)))
  (default-to false (map-get? name-taken name))
)

(define-read-only (get-registry-count)
  (var-get registry-count)
)

(define-public (register (name (string-ascii 50)))
  (begin
    (asserts! (not (is-name-taken name)) (err u101))
    (asserts! (is-none (get-username tx-sender)) (err u102))
    (map-set usernames tx-sender name)
    (map-set name-taken name true)
    (var-set registry-count (+ (var-get registry-count) u1))
    (ok name)
  )
)
