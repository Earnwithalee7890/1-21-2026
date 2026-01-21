;; Auction House - Clarity 4
;; Create and bid on auctions

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1700))
(define-constant err-unauthorized (err u1701))
(define-constant err-auction-ended (err u1702))
(define-constant err-bid-too-low (err u1703))

(define-data-var auction-nonce uint u0)
(define-data-var min-bid-increment uint u100000)

(define-map auctions
    uint
    {
        seller: principal,
        item-name: (string-utf8 100),
        description: (string-utf8 300),
        start-price: uint,
        current-bid: uint,
        highest-bidder: (optional principal),
        end-block: uint,
        settled: bool
    }
)

(define-read-only (get-auction (auction-id uint))
    (map-get? auctions auction-id)
)

(define-read-only (is-auction-active (auction-id uint))
    (match (get-auction auction-id)
        a (and (not (get settled a)) (<= stacks-block-height (get end-block a)))
        false
    )
)

(define-public (create-auction
    (item-name (string-utf8 100))
    (description (string-utf8 300))
    (start-price uint)
    (duration uint)
)
    (let (
        (auction-id (var-get auction-nonce))
    )
        (map-set auctions auction-id {
            seller: tx-sender,
            item-name: item-name,
            description: description,
            start-price: start-price,
            current-bid: u0,
            highest-bidder: none,
            end-block: (+ stacks-block-height duration),
            settled: false
        })
        (var-set auction-nonce (+ auction-id u1))
        (ok auction-id)
    )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let (
        (auction (unwrap! (get-auction auction-id) err-not-found))
        (min-bid (+ (get current-bid auction) (var-get min-bid-increment)))
    )
        (asserts! (is-auction-active auction-id) err-auction-ended)
        (asserts! (>= bid-amount min-bid) err-bid-too-low)
        (asserts! (>= bid-amount (get start-price auction)) err-bid-too-low)
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        (match (get highest-bidder auction)
            prev-bidder (try! (as-contract (stx-transfer? (get current-bid auction) tx-sender prev-bidder)))
            true
        )
        (ok (map-set auctions auction-id
            (merge auction {current-bid: bid-amount, highest-bidder: (some tx-sender)})
        ))
    )
)

(define-public (settle-auction (auction-id uint))
    (let (
        (auction (unwrap! (get-auction auction-id) err-not-found))
        (winner (unwrap! (get highest-bidder auction) err-not-found))
    )
        (asserts! (> stacks-block-height (get end-block auction)) err-auction-ended)
        (asserts! (not (get settled auction)) err-already-completed)
        (map-set auctions auction-id (merge auction {settled: true}))
        (try! (as-contract (stx-transfer? (get current-bid auction) tx-sender (get seller auction))))
        (ok winner)
    )
)
