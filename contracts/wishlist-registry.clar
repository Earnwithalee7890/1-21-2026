;; Wishlist Registry - Clarity 4
;; Create wishlists and receive gifts

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u2600))
(define-constant err-unauthorized (err u2601))
(define-constant err-already-fulfilled (err u2602))

(define-data-var wishlist-nonce uint u0)
(define-data-var item-nonce uint u0)

(define-map wishlists
    uint
    {
        owner: principal,
        name: (string-utf8 50),
        description: (string-utf8 200),
        created-at: uint,
        public: bool
    }
)

(define-map wishlist-items
    uint
    {
        wishlist-id: uint,
        name: (string-utf8 100),
        price: uint,
        fulfilled: bool,
        gifter: (optional principal)
    }
)

(define-read-only (get-wishlist (wishlist-id uint))
    (map-get? wishlists wishlist-id)
)

(define-read-only (get-item (item-id uint))
    (map-get? wishlist-items item-id)
)

(define-public (create-wishlist
    (name (string-utf8 50))
    (description (string-utf8 200))
    (public bool)
)
    (let (
        (wishlist-id (var-get wishlist-nonce))
    )
        (map-set wishlists wishlist-id {
            owner: tx-sender,
            name: name,
            description: description,
            created-at: stacks-block-height,
            public: public
        })
        (var-set wishlist-nonce (+ wishlist-id u1))
        (ok wishlist-id)
    )
)

(define-public (add-item
    (wishlist-id uint)
    (name (string-utf8 100))
    (price uint)
)
    (let (
        (wishlist (unwrap! (get-wishlist wishlist-id) err-not-found))
        (item-id (var-get item-nonce))
    )
        (asserts! (is-eq tx-sender (get owner wishlist)) err-unauthorized)
        (map-set wishlist-items item-id {
            wishlist-id: wishlist-id,
            name: name,
            price: price,
            fulfilled: false,
            gifter: none
        })
        (var-set item-nonce (+ item-id u1))
        (ok item-id)
    )
)

(define-public (gift-item (item-id uint))
    (let (
        (item (unwrap! (get-item item-id) err-not-found))
        (wishlist (unwrap! (get-wishlist (get wishlist-id item)) err-not-found))
    )
        (asserts! (not (get fulfilled item)) err-already-fulfilled)
        (try! (stx-transfer? (get price item) tx-sender (get owner wishlist)))
        (ok (map-set wishlist-items item-id
            (merge item {fulfilled: true, gifter: (some tx-sender)})
        ))
    )
)
