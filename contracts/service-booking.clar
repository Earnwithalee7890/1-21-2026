;; Service Booking - Clarity 4
;; Book and manage service appointments

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u4000))
(define-constant err-unauthorized (err u4001))
(define-constant err-already-booked (err u4002))

(define-data-var booking-nonce uint u0)
(define-data-var platform-fee uint u200)

(define-map services
    uint
    {
        provider: principal,
        name: (string-utf8 100),
        description: (string-utf8 300),
        price: uint,
        available: bool
    }
)

(define-map bookings
    uint
    {
        service-id: uint,
        client: principal,
        booked-at: uint,
        completed: bool,
        paid: bool
    }
)

(define-read-only (get-service (service-id uint))
    (map-get? services service-id)
)

(define-read-only (get-booking (booking-id uint))
    (map-get? bookings booking-id)
)

(define-public (create-service
    (name (string-utf8 100))
    (description (string-utf8 300))
    (price uint)
)
    (let (
        (service-id (var-get booking-nonce))
    )
        (map-set services service-id {
            provider: tx-sender,
            name: name,
            description: description,
            price: price,
            available: true
        })
        (var-set booking-nonce (+ service-id u1))
        (ok service-id)
    )
)

(define-public (book-service (service-id uint))
    (let (
        (service (unwrap! (get-service service-id) err-not-found))
        (booking-id (var-get booking-nonce))
    )
        (asserts! (get available service) err-already-booked)
        (try! (stx-transfer? (get price service) tx-sender (as-contract tx-sender)))
        (map-set bookings booking-id {
            service-id: service-id,
            client: tx-sender,
            booked-at: stacks-block-height,
            completed: false,
            paid: false
        })
        (var-set booking-nonce (+ booking-id u1))
        (ok booking-id)
    )
)

(define-public (complete-booking (booking-id uint))
    (let (
        (booking (unwrap! (get-booking booking-id) err-not-found))
        (service (unwrap! (get-service (get service-id booking)) err-not-found))
        (fee (/ (* (get price service) (var-get platform-fee)) u10000))
        (payment (- (get price service) fee))
    )
        (asserts! (is-eq tx-sender (get client booking)) err-unauthorized)
        (map-set bookings booking-id (merge booking {completed: true, paid: true}))
        (try! (as-contract (stx-transfer? payment tx-sender (get provider service))))
        (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
        (ok true)
    )
)
