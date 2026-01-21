;; Time Capsule - Clarity 4
;; Lock messages to be revealed later

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u2000))
(define-constant err-unauthorized (err u2001))
(define-constant err-not-unlocked (err u2002))
(define-constant err-already-revealed (err u2003))

(define-data-var capsule-nonce uint u0)

(define-map time-capsules
    uint
    {
        creator: principal,
        recipient: principal,
        message: (string-utf8 500),
        unlock-block: uint,
        revealed: bool,
        deposit: uint
    }
)

(define-read-only (get-capsule (capsule-id uint))
    (map-get? time-capsules capsule-id)
)

(define-read-only (is-unlocked (capsule-id uint))
    (match (get-capsule capsule-id)
        c (>= stacks-block-height (get unlock-block c))
        false
    )
)

(define-public (create-capsule
    (recipient principal)
    (message (string-utf8 500))
    (unlock-delay uint)
    (deposit uint)
)
    (let (
        (capsule-id (var-get capsule-nonce))
    )
        (try! (stx-transfer? deposit tx-sender (as-contract tx-sender)))
        (map-set time-capsules capsule-id {
            creator: tx-sender,
            recipient: recipient,
            message: message,
            unlock-block: (+ stacks-block-height unlock-delay),
            revealed: false,
            deposit: deposit
        })
        (var-set capsule-nonce (+ capsule-id u1))
        (ok capsule-id)
    )
)

(define-public (reveal-capsule (capsule-id uint))
    (let (
        (capsule (unwrap! (get-capsule capsule-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get recipient capsule)) err-unauthorized)
        (asserts! (is-unlocked capsule-id) err-not-unlocked)
        (asserts! (not (get revealed capsule)) err-already-revealed)
        (map-set time-capsules capsule-id (merge capsule {revealed: true}))
        (try! (as-contract (stx-transfer? (get deposit capsule) tx-sender (get recipient capsule))))
        (ok (get message capsule))
    )
)
