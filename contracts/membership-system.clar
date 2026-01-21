;; Membership System - Clarity 4
;; Manage paid memberships

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1300))
(define-constant err-unauthorized (err u1301))
(define-constant err-already-member (err u1302))

(define-data-var membership-fee uint u1000000)
(define-data-var membership-duration uint u4320)
(define-data-var member-count uint u0)

(define-map members
    principal
    {
        joined-at: uint,
        expires-at: uint,
        tier: (string-utf8 20),
        active: bool
    }
)

(define-read-only (get-member (user principal))
    (map-get? members user)
)

(define-read-only (is-active-member (user principal))
    (match (get-member user)
        m (and (get active m) (>= (get expires-at m) block-height))
        false
    )
)

(define-read-only (get-member-count)
    (var-get member-count)
)

(define-public (join)
    (begin
        (asserts! (is-none (get-member tx-sender)) err-already-member)
        (try! (stx-transfer? (var-get membership-fee) tx-sender contract-owner))
        (var-set member-count (+ (var-get member-count) u1))
        (ok (map-set members tx-sender {
            joined-at: block-height,
            expires-at: (+ block-height (var-get membership-duration)),
            tier: u"standard",
            active: true
        }))
    )
)

(define-public (renew)
    (let (
        (member (unwrap! (get-member tx-sender) err-not-found))
        (new-expiry (+ (get expires-at member) (var-get membership-duration)))
    )
        (try! (stx-transfer? (var-get membership-fee) tx-sender contract-owner))
        (ok (map-set members tx-sender
            (merge member {expires-at: new-expiry})
        ))
    )
)

(define-public (cancel)
    (let (
        (member (unwrap! (get-member tx-sender) err-not-found))
    )
        (ok (map-set members tx-sender (merge member {active: false})))
    )
)

(define-public (set-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (ok (var-set membership-fee new-fee))
    )
)
