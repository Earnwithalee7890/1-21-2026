;; Escrow Service - Clarity 4
;; Secure escrow for peer-to-peer trades

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u6000))
(define-constant err-unauthorized (err u6001))
(define-constant err-invalid-state (err u6002))

(define-data-var escrow-nonce uint u0)
(define-data-var arbiter-fee uint u100)

(define-map escrows
    uint
    {
        buyer: principal,
        seller: principal,
        arbiter: principal,
        amount: uint,
        description: (string-utf8 200),
        status: (string-utf8 20),
        created-at: uint
    }
)

(define-read-only (get-escrow (escrow-id uint))
    (map-get? escrows escrow-id)
)

(define-read-only (get-escrow-count)
    (var-get escrow-nonce)
)

(define-public (create-escrow
    (seller principal)
    (arbiter principal)
    (amount uint)
    (description (string-utf8 200))
)
    (let (
        (escrow-id (var-get escrow-nonce))
    )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set escrows escrow-id {
            buyer: tx-sender,
            seller: seller,
            arbiter: arbiter,
            amount: amount,
            description: description,
            status: u"funded",
            created-at: stacks-block-height
        })
        (var-set escrow-nonce (+ escrow-id u1))
        (ok escrow-id)
    )
)

(define-public (release-to-seller (escrow-id uint))
    (let (
        (escrow (unwrap! (get-escrow escrow-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get buyer escrow)) err-unauthorized)
        (map-set escrows escrow-id (merge escrow {status: u"released"}))
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))
        (ok true)
    )
)

(define-public (refund-to-buyer (escrow-id uint))
    (let (
        (escrow (unwrap! (get-escrow escrow-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get seller escrow)) err-unauthorized)
        (map-set escrows escrow-id (merge escrow {status: u"refunded"}))
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
        (ok true)
    )
)

(define-public (resolve-dispute (escrow-id uint) (release-to-seller bool))
    (let (
        (escrow (unwrap! (get-escrow escrow-id) err-not-found))
        (fee (/ (* (get amount escrow) (var-get arbiter-fee)) u10000))
        (payout (- (get amount escrow) fee))
        (recipient (if release-to-seller (get seller escrow) (get buyer escrow)))
    )
        (asserts! (is-eq tx-sender (get arbiter escrow)) err-unauthorized)
        (map-set escrows escrow-id (merge escrow {status: u"resolved"}))
        (try! (as-contract (stx-transfer? payout tx-sender recipient)))
        (try! (as-contract (stx-transfer? fee tx-sender (get arbiter escrow))))
        (ok true)
    )
)
