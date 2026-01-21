;; Subscription Service - Clarity 4
;; Recurring subscription payments

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u8000))
(define-constant err-unauthorized (err u8001))
(define-constant err-already-subscribed (err u8002))
(define-constant err-expired (err u8003))

(define-data-var plan-nonce uint u0)

(define-map plans
    uint
    {
        creator: principal,
        name: (string-utf8 50),
        description: (string-utf8 200),
        price: uint,
        duration: uint,
        active: bool
    }
)

(define-map subscriptions
    {plan-id: uint, subscriber: principal}
    {start-block: uint, end-block: uint, active: bool}
)

(define-read-only (get-plan (plan-id uint))
    (map-get? plans plan-id)
)

(define-read-only (get-subscription (plan-id uint) (subscriber principal))
    (map-get? subscriptions {plan-id: plan-id, subscriber: subscriber})
)

(define-read-only (is-subscribed (plan-id uint) (subscriber principal))
    (match (get-subscription plan-id subscriber)
        sub (and (get active sub) (>= (get end-block sub) stacks-block-height))
        false
    )
)

(define-public (create-plan
    (name (string-utf8 50))
    (description (string-utf8 200))
    (price uint)
    (duration uint)
)
    (let (
        (plan-id (var-get plan-nonce))
    )
        (map-set plans plan-id {
            creator: tx-sender,
            name: name,
            description: description,
            price: price,
            duration: duration,
            active: true
        })
        (var-set plan-nonce (+ plan-id u1))
        (ok plan-id)
    )
)

(define-public (subscribe (plan-id uint))
    (let (
        (plan (unwrap! (get-plan plan-id) err-not-found))
    )
        (asserts! (get active plan) err-expired)
        (try! (stx-transfer? (get price plan) tx-sender (get creator plan)))
        (ok (map-set subscriptions {plan-id: plan-id, subscriber: tx-sender}
            {
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height (get duration plan)),
                active: true
            }
        ))
    )
)

(define-public (renew (plan-id uint))
    (let (
        (plan (unwrap! (get-plan plan-id) err-not-found))
        (sub (unwrap! (get-subscription plan-id tx-sender) err-not-found))
        (new-end (+ (get end-block sub) (get duration plan)))
    )
        (asserts! (get active plan) err-expired)
        (try! (stx-transfer? (get price plan) tx-sender (get creator plan)))
        (ok (map-set subscriptions {plan-id: plan-id, subscriber: tx-sender}
            (merge sub {end-block: new-end})
        ))
    )
)

(define-public (cancel-subscription (plan-id uint))
    (let (
        (sub (unwrap! (get-subscription plan-id tx-sender) err-not-found))
    )
        (ok (map-set subscriptions {plan-id: plan-id, subscriber: tx-sender}
            (merge sub {active: false})
        ))
    )
)
