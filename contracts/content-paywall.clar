;; Content Paywall - Clarity 4
;; Pay to access content

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u2200))
(define-constant err-unauthorized (err u2201))
(define-constant err-already-purchased (err u2202))

(define-data-var content-nonce uint u0)
(define-data-var platform-fee uint u200)

(define-map content-items
    uint
    {
        creator: principal,
        title: (string-utf8 100),
        preview: (string-utf8 200),
        price: uint,
        purchase-count: uint,
        active: bool
    }
)

(define-map purchases
    {content-id: uint, buyer: principal}
    {purchased-at: uint, price-paid: uint}
)

(define-read-only (get-content (content-id uint))
    (map-get? content-items content-id)
)

(define-read-only (has-purchased (content-id uint) (buyer principal))
    (is-some (map-get? purchases {content-id: content-id, buyer: buyer}))
)

(define-public (create-content
    (title (string-utf8 100))
    (preview (string-utf8 200))
    (price uint)
)
    (let (
        (content-id (var-get content-nonce))
    )
        (map-set content-items content-id {
            creator: tx-sender,
            title: title,
            preview: preview,
            price: price,
            purchase-count: u0,
            active: true
        })
        (var-set content-nonce (+ content-id u1))
        (ok content-id)
    )
)

(define-public (purchase-content (content-id uint))
    (let (
        (content (unwrap! (get-content content-id) err-not-found))
        (fee (/ (* (get price content) (var-get platform-fee)) u10000))
        (creator-share (- (get price content) fee))
    )
        (asserts! (get active content) err-not-found)
        (asserts! (not (has-purchased content-id tx-sender)) err-already-purchased)
        (try! (stx-transfer? creator-share tx-sender (get creator content)))
        (try! (stx-transfer? fee tx-sender contract-owner))
        (map-set content-items content-id
            (merge content {purchase-count: (+ (get purchase-count content) u1)})
        )
        (ok (map-set purchases {content-id: content-id, buyer: tx-sender}
            {purchased-at: stacks-block-height, price-paid: (get price content)}
        ))
    )
)

(define-public (deactivate-content (content-id uint))
    (let (
        (content (unwrap! (get-content content-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get creator content)) err-unauthorized)
        (ok (map-set content-items content-id (merge content {active: false})))
    )
)
