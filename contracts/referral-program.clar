;; Referral Program - Clarity 4
;; Track and reward referrals

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1400))
(define-constant err-unauthorized (err u1401))
(define-constant err-self-referral (err u1402))
(define-constant err-already-referred (err u1403))

(define-data-var referral-bonus uint u100000)
(define-data-var total-referrals uint u0)

(define-map referrals
    principal
    {referrer: principal, referred-at: uint, bonus-paid: bool}
)

(define-map referrer-stats
    principal
    {count: uint, earnings: uint}
)

(define-read-only (get-referral (user principal))
    (map-get? referrals user)
)

(define-read-only (get-referrer-stats (referrer principal))
    (default-to {count: u0, earnings: u0}
        (map-get? referrer-stats referrer)
    )
)

(define-read-only (get-total-referrals)
    (var-get total-referrals)
)

(define-public (register-referral (referrer principal))
    (let (
        (stats (get-referrer-stats referrer))
    )
        (asserts! (is-none (get-referral tx-sender)) err-already-referred)
        (asserts! (not (is-eq tx-sender referrer)) err-self-referral)
        (map-set referrals tx-sender {
            referrer: referrer,
            referred-at: block-height,
            bonus-paid: false
        })
        (map-set referrer-stats referrer
            (merge stats {count: (+ (get count stats) u1)})
        )
        (var-set total-referrals (+ (var-get total-referrals) u1))
        (ok true)
    )
)

(define-public (pay-referral-bonus (user principal))
    (let (
        (ref (unwrap! (get-referral user) err-not-found))
        (stats (get-referrer-stats (get referrer ref)))
        (bonus (var-get referral-bonus))
    )
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (not (get bonus-paid ref)) err-already-referred)
        (try! (stx-transfer? bonus tx-sender (get referrer ref)))
        (map-set referrals user (merge ref {bonus-paid: true}))
        (ok (map-set referrer-stats (get referrer ref)
            (merge stats {earnings: (+ (get earnings stats) bonus)})
        ))
    )
)

(define-public (set-bonus (new-bonus uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (ok (var-set referral-bonus new-bonus))
    )
)
