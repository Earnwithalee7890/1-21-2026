;; Member Registry
;; Track members with join dates

(define-data-var member-count uint u0)
(define-data-var admin principal tx-sender)

(define-map members principal 
  {
    joined: uint,
    active: bool
  }
)

(define-read-only (get-member (user principal))
  (map-get? members user)
)

(define-read-only (is-member (user principal))
  (is-some (get-member user))
)

(define-read-only (get-member-count)
  (var-get member-count)
)

(define-public (join)
  (begin
    (asserts! (not (is-member tx-sender)) (err u101))
    (map-set members tx-sender 
      {
        joined: block-height,
        active: true
      }
    )
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)
  )
)

(define-public (leave)
  (let
    (
      (member (unwrap! (get-member tx-sender) (err u102)))
    )
    (map-set members tx-sender 
      {
        joined: (get joined member),
        active: false
      }
    )
    (ok true)
  )
)
