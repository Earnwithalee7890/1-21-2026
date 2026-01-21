;; Expense Tracker - Clarity 4
;; Track and categorize expenses

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u2500))
(define-constant err-unauthorized (err u2501))

(define-data-var expense-nonce uint u0)

(define-map expenses
    uint
    {
        owner: principal,
        amount: uint,
        category: (string-utf8 30),
        description: (string-utf8 100),
        recorded-at: uint
    }
)

(define-map monthly-totals
    {owner: principal, month: uint}
    {total: uint, count: uint}
)

(define-read-only (get-expense (expense-id uint))
    (map-get? expenses expense-id)
)

(define-read-only (get-monthly-total (owner principal) (month uint))
    (default-to {total: u0, count: u0}
        (map-get? monthly-totals {owner: owner, month: month})
    )
)

(define-public (record-expense
    (amount uint)
    (category (string-utf8 30))
    (description (string-utf8 100))
)
    (let (
        (expense-id (var-get expense-nonce))
        (month (/ stacks-block-height u4320))
        (current-monthly (get-monthly-total tx-sender month))
    )
        (map-set expenses expense-id {
            owner: tx-sender,
            amount: amount,
            category: category,
            description: description,
            recorded-at: stacks-block-height
        })
        (map-set monthly-totals {owner: tx-sender, month: month}
            {
                total: (+ (get total current-monthly) amount),
                count: (+ (get count current-monthly) u1)
            }
        )
        (var-set expense-nonce (+ expense-id u1))
        (ok expense-id)
    )
)

(define-public (delete-expense (expense-id uint))
    (let (
        (expense (unwrap! (get-expense expense-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get owner expense)) err-unauthorized)
        (ok (map-delete expenses expense-id))
    )
)
