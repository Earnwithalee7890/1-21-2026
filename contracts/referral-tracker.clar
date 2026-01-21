;; Referral Tracker
;; Track referrals between users

(define-data-var referral-count uint u0)

(define-map referrals principal principal)
(define-map referral-counts principal uint)

(define-read-only (get-referrer (user principal))
  (map-get? referrals user)
)

(define-read-only (get-referral-count (user principal))
  (default-to u0 (map-get? referral-counts user))
)

(define-read-only (get-total-referrals)
  (var-get referral-count)
)

(define-public (register-referral (referrer principal))
  (begin
    (asserts! (is-none (get-referrer tx-sender)) (err u101))
    (asserts! (not (is-eq tx-sender referrer)) (err u102))
    (map-set referrals tx-sender referrer)
    (map-set referral-counts referrer 
      (+ (get-referral-count referrer) u1)
    )
    (var-set referral-count (+ (var-get referral-count) u1))
    (ok true)
  )
)
