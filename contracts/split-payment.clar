;; Split Payment - Clarity 4
;; Split payments between multiple recipients

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1900))
(define-constant err-unauthorized (err u1901))
(define-constant err-invalid-split (err u1902))

(define-data-var split-nonce uint u0)

(define-map payment-splits
    uint
    {
        creator: principal,
        name: (string-utf8 50),
        recipient-1: principal,
        recipient-2: principal,
        share-1: uint,
        share-2: uint,
        total-received: uint
    }
)

(define-read-only (get-split (split-id uint))
    (map-get? payment-splits split-id)
)

(define-public (create-split
    (name (string-utf8 50))
    (recipient-1 principal)
    (recipient-2 principal)
    (share-1 uint)
    (share-2 uint)
)
    (let (
        (split-id (var-get split-nonce))
    )
        (asserts! (is-eq (+ share-1 share-2) u10000) err-invalid-split)
        (map-set payment-splits split-id {
            creator: tx-sender,
            name: name,
            recipient-1: recipient-1,
            recipient-2: recipient-2,
            share-1: share-1,
            share-2: share-2,
            total-received: u0
        })
        (var-set split-nonce (+ split-id u1))
        (ok split-id)
    )
)

(define-public (send-to-split (split-id uint) (amount uint))
    (let (
        (split (unwrap! (get-split split-id) err-not-found))
        (amount-1 (/ (* amount (get share-1 split)) u10000))
        (amount-2 (- amount amount-1))
    )
        (try! (stx-transfer? amount-1 tx-sender (get recipient-1 split)))
        (try! (stx-transfer? amount-2 tx-sender (get recipient-2 split)))
        (ok (map-set payment-splits split-id
            (merge split {total-received: (+ (get total-received split) amount)})
        ))
    )
)
