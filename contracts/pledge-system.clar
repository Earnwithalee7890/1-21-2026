;; Pledge System - Clarity 4
;; Make and fulfill pledges

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u2100))
(define-constant err-unauthorized (err u2101))
(define-constant err-already-fulfilled (err u2102))

(define-data-var pledge-nonce uint u0)

(define-map pledges
    uint
    {
        pledger: principal,
        beneficiary: principal,
        description: (string-utf8 200),
        amount: uint,
        deadline: uint,
        fulfilled: bool,
        created-at: uint
    }
)

(define-read-only (get-pledge (pledge-id uint))
    (map-get? pledges pledge-id)
)

(define-read-only (is-overdue (pledge-id uint))
    (match (get-pledge pledge-id)
        p (and (not (get fulfilled p)) (> stacks-block-height (get deadline p)))
        false
    )
)

(define-public (make-pledge
    (beneficiary principal)
    (description (string-utf8 200))
    (amount uint)
    (deadline uint)
)
    (let (
        (pledge-id (var-get pledge-nonce))
    )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set pledges pledge-id {
            pledger: tx-sender,
            beneficiary: beneficiary,
            description: description,
            amount: amount,
            deadline: (+ stacks-block-height deadline),
            fulfilled: false,
            created-at: stacks-block-height
        })
        (var-set pledge-nonce (+ pledge-id u1))
        (ok pledge-id)
    )
)

(define-public (fulfill-pledge (pledge-id uint))
    (let (
        (pledge (unwrap! (get-pledge pledge-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get pledger pledge)) err-unauthorized)
        (asserts! (not (get fulfilled pledge)) err-already-fulfilled)
        (map-set pledges pledge-id (merge pledge {fulfilled: true}))
        (try! (as-contract (stx-transfer? (get amount pledge) tx-sender (get beneficiary pledge))))
        (ok true)
    )
)

(define-public (cancel-pledge (pledge-id uint))
    (let (
        (pledge (unwrap! (get-pledge pledge-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get pledger pledge)) err-unauthorized)
        (asserts! (not (get fulfilled pledge)) err-already-fulfilled)
        (asserts! (> stacks-block-height (get deadline pledge)) err-unauthorized)
        (try! (as-contract (stx-transfer? (get amount pledge) tx-sender (get pledger pledge))))
        (ok (map-delete pledges pledge-id))
    )
)
