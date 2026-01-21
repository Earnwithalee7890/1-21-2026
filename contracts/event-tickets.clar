;; Event Tickets - Clarity 4
;; Create events and sell tickets

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u2700))
(define-constant err-unauthorized (err u2701))
(define-constant err-sold-out (err u2702))
(define-constant err-already-used (err u2703))

(define-data-var event-nonce uint u0)
(define-data-var ticket-nonce uint u0)
(define-data-var platform-fee uint u250)

(define-map events
    uint
    {
        organizer: principal,
        name: (string-utf8 100),
        description: (string-utf8 300),
        ticket-price: uint,
        max-tickets: uint,
        sold-count: uint,
        event-block: uint,
        active: bool
    }
)

(define-map tickets
    uint
    {
        event-id: uint,
        holder: principal,
        purchased-at: uint,
        used: bool
    }
)

(define-read-only (get-event (event-id uint))
    (map-get? events event-id)
)

(define-read-only (get-ticket (ticket-id uint))
    (map-get? tickets ticket-id)
)

(define-read-only (tickets-available (event-id uint))
    (match (get-event event-id)
        e (- (get max-tickets e) (get sold-count e))
        u0
    )
)

(define-public (create-event
    (name (string-utf8 100))
    (description (string-utf8 300))
    (ticket-price uint)
    (max-tickets uint)
    (event-in-blocks uint)
)
    (let (
        (event-id (var-get event-nonce))
    )
        (map-set events event-id {
            organizer: tx-sender,
            name: name,
            description: description,
            ticket-price: ticket-price,
            max-tickets: max-tickets,
            sold-count: u0,
            event-block: (+ stacks-block-height event-in-blocks),
            active: true
        })
        (var-set event-nonce (+ event-id u1))
        (ok event-id)
    )
)

(define-public (buy-ticket (event-id uint))
    (let (
        (event (unwrap! (get-event event-id) err-not-found))
        (ticket-id (var-get ticket-nonce))
        (fee (/ (* (get ticket-price event) (var-get platform-fee)) u10000))
        (organizer-share (- (get ticket-price event) fee))
    )
        (asserts! (get active event) err-not-found)
        (asserts! (< (get sold-count event) (get max-tickets event)) err-sold-out)
        (try! (stx-transfer? organizer-share tx-sender (get organizer event)))
        (try! (stx-transfer? fee tx-sender contract-owner))
        (map-set events event-id
            (merge event {sold-count: (+ (get sold-count event) u1)})
        )
        (map-set tickets ticket-id {
            event-id: event-id,
            holder: tx-sender,
            purchased-at: stacks-block-height,
            used: false
        })
        (var-set ticket-nonce (+ ticket-id u1))
        (ok ticket-id)
    )
)

(define-public (use-ticket (ticket-id uint))
    (let (
        (ticket (unwrap! (get-ticket ticket-id) err-not-found))
        (event (unwrap! (get-event (get event-id ticket)) err-not-found))
    )
        (asserts! (is-eq tx-sender (get organizer event)) err-unauthorized)
        (asserts! (not (get used ticket)) err-already-used)
        (ok (map-set tickets ticket-id (merge ticket {used: true})))
    )
)
