;; Invoice System - Clarity 4
;; Create and pay invoices

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1100))
(define-constant err-unauthorized (err u1101))
(define-constant err-already-paid (err u1102))

(define-data-var invoice-nonce uint u0)

(define-map invoices
    uint
    {
        issuer: principal,
        client: principal,
        description: (string-utf8 200),
        amount: uint,
        due-block: uint,
        paid: bool,
        paid-at: uint
    }
)

(define-read-only (get-invoice (invoice-id uint))
    (map-get? invoices invoice-id)
)

(define-read-only (is-overdue (invoice-id uint))
    (match (get-invoice invoice-id)
        inv (and (not (get paid inv)) (> block-height (get due-block inv)))
        false
    )
)

(define-public (create-invoice
    (client principal)
    (description (string-utf8 200))
    (amount uint)
    (due-blocks uint)
)
    (let (
        (invoice-id (var-get invoice-nonce))
    )
        (map-set invoices invoice-id {
            issuer: tx-sender,
            client: client,
            description: description,
            amount: amount,
            due-block: (+ block-height due-blocks),
            paid: false,
            paid-at: u0
        })
        (var-set invoice-nonce (+ invoice-id u1))
        (ok invoice-id)
    )
)

(define-public (pay-invoice (invoice-id uint))
    (let (
        (inv (unwrap! (get-invoice invoice-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get client inv)) err-unauthorized)
        (asserts! (not (get paid inv)) err-already-paid)
        (try! (stx-transfer? (get amount inv) tx-sender (get issuer inv)))
        (ok (map-set invoices invoice-id
            (merge inv {paid: true, paid-at: block-height})
        ))
    )
)

(define-public (cancel-invoice (invoice-id uint))
    (let (
        (inv (unwrap! (get-invoice invoice-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get issuer inv)) err-unauthorized)
        (asserts! (not (get paid inv)) err-already-paid)
        (ok (map-delete invoices invoice-id))
    )
)
