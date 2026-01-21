;; Simple Token
;; Basic fungible token

(define-fungible-token simple-token)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance simple-token account))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply simple-token))
)

(define-public (transfer (amount uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-owner-only)
    (ft-transfer? simple-token amount sender recipient)
  )
)

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ft-mint? simple-token amount recipient)
  )
)
