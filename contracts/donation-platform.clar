;; Donation Platform - Clarity 4
;; Accept and track donations

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u9000))
(define-constant err-unauthorized (err u9001))

(define-data-var cause-nonce uint u0)
(define-data-var platform-fee uint u200)

(define-map causes
    uint
    {
        owner: principal,
        name: (string-utf8 100),
        description: (string-utf8 500),
        goal: uint,
        raised: uint,
        active: bool
    }
)

(define-map donations
    {cause-id: uint, donor: principal}
    {amount: uint, donated-at: uint}
)

(define-read-only (get-cause (cause-id uint))
    (map-get? causes cause-id)
)

(define-read-only (get-donation (cause-id uint) (donor principal))
    (map-get? donations {cause-id: cause-id, donor: donor})
)

(define-public (create-cause
    (name (string-utf8 100))
    (description (string-utf8 500))
    (goal uint)
)
    (let (
        (cause-id (var-get cause-nonce))
    )
        (map-set causes cause-id {
            owner: tx-sender,
            name: name,
            description: description,
            goal: goal,
            raised: u0,
            active: true
        })
        (var-set cause-nonce (+ cause-id u1))
        (ok cause-id)
    )
)

(define-public (donate (cause-id uint) (amount uint))
    (let (
        (cause (unwrap! (get-cause cause-id) err-not-found))
        (fee (/ (* amount (var-get platform-fee)) u10000))
        (donation (- amount fee))
    )
        (asserts! (get active cause) err-not-found)
        (try! (stx-transfer? donation tx-sender (get owner cause)))
        (try! (stx-transfer? fee tx-sender contract-owner))
        (map-set causes cause-id
            (merge cause {raised: (+ (get raised cause) donation)})
        )
        (ok (map-set donations {cause-id: cause-id, donor: tx-sender}
            {amount: amount, donated-at: block-height}
        ))
    )
)

(define-public (close-cause (cause-id uint))
    (let (
        (cause (unwrap! (get-cause cause-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get owner cause)) err-unauthorized)
        (ok (map-set causes cause-id (merge cause {active: false})))
    )
)
