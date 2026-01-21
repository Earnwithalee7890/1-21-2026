;; Freelance Marketplace - Clarity 4
;; Create and manage freelance gigs

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u3000))
(define-constant err-unauthorized (err u3001))
(define-constant err-already-taken (err u3002))

(define-data-var gig-nonce uint u0)
(define-data-var platform-fee uint u250)

(define-map gigs
    uint
    {
        freelancer: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
        price: uint,
        active: bool,
        buyer: (optional principal),
        delivered: bool
    }
)

(define-read-only (get-gig (gig-id uint))
    (map-get? gigs gig-id)
)

(define-read-only (get-gig-count)
    (var-get gig-nonce)
)

(define-public (create-gig
    (title (string-utf8 100))
    (description (string-utf8 500))
    (price uint)
)
    (let (
        (gig-id (var-get gig-nonce))
    )
        (map-set gigs gig-id {
            freelancer: tx-sender,
            title: title,
            description: description,
            price: price,
            active: true,
            buyer: none,
            delivered: false
        })
        (var-set gig-nonce (+ gig-id u1))
        (ok gig-id)
    )
)

(define-public (purchase-gig (gig-id uint))
    (let (
        (gig (unwrap! (get-gig gig-id) err-not-found))
    )
        (asserts! (get active gig) err-already-taken)
        (asserts! (is-none (get buyer gig)) err-already-taken)
        (try! (stx-transfer? (get price gig) tx-sender (as-contract tx-sender)))
        (ok (map-set gigs gig-id
            (merge gig {buyer: (some tx-sender)})
        ))
    )
)

(define-public (deliver-gig (gig-id uint))
    (let (
        (gig (unwrap! (get-gig gig-id) err-not-found))
        (buyer (unwrap! (get buyer gig) err-not-found))
        (fee (/ (* (get price gig) (var-get platform-fee)) u10000))
        (payment (- (get price gig) fee))
    )
        (asserts! (is-eq tx-sender buyer) err-unauthorized)
        (map-set gigs gig-id (merge gig {delivered: true}))
        (try! (as-contract (stx-transfer? payment tx-sender (get freelancer gig))))
        (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
        (ok true)
    )
)

(define-public (cancel-gig (gig-id uint))
    (let (
        (gig (unwrap! (get-gig gig-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get freelancer gig)) err-unauthorized)
        (asserts! (is-none (get buyer gig)) err-already-taken)
        (ok (map-set gigs gig-id (merge gig {active: false})))
    )
)
