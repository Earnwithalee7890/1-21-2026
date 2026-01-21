;; Balance Store
;; Track balances for users

(define-data-var admin principal tx-sender)
(define-data-var total-balance uint u0)

(define-map balances principal uint)

(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? balances user))
)

(define-read-only (get-total-balance)
  (var-get total-balance)
)

(define-public (deposit (amount uint))
  (let
    (
      (current (get-balance tx-sender))
    )
    (map-set balances tx-sender (+ current amount))
    (var-set total-balance (+ (var-get total-balance) amount))
    (ok (+ current amount))
  )
)

(define-public (withdraw (amount uint))
  (let
    (
      (current (get-balance tx-sender))
    )
    (asserts! (>= current amount) (err u101))
    (map-set balances tx-sender (- current amount))
    (var-set total-balance (- (var-get total-balance) amount))
    (ok (- current amount))
  )
)
