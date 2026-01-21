;; Subscription Service
;; Recurring payment subscriptions with tiered pricing

(define-map subscription-plans
  { plan-id: uint }
  {
    creator: principal,
    name: (string-ascii 50),
    price: uint,
    duration-blocks: uint,
    features: (string-ascii 200),
    active: bool,
    subscriber-count: uint
  }
)

(define-map subscriptions
  { subscriber: principal, plan-id: uint }
  {
    start-block: uint,
    end-block: uint,
    auto-renew: bool,
    payments-made: uint
  }
)

(define-data-var plan-nonce uint u0)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PLAN-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-SUBSCRIBED (err u102))
(define-constant ERR-NOT-SUBSCRIBED (err u103))
(define-constant ERR-PLAN-INACTIVE (err u104))
(define-constant ERR-SUBSCRIPTION-ACTIVE (err u105))

(define-read-only (get-plan (plan-id uint))
  (map-get? subscription-plans { plan-id: plan-id })
)

(define-read-only (get-subscription (subscriber principal) (plan-id uint))
  (map-get? subscriptions { subscriber: subscriber, plan-id: plan-id })
)

(define-read-only (is-subscribed (subscriber principal) (plan-id uint))
  (match (get-subscription subscriber plan-id)
    sub (>= (get end-block sub) block-height)
    false
  )
)

(define-read-only (get-remaining-time (subscriber principal) (plan-id uint))
  (match (get-subscription subscriber plan-id)
    sub (if (>= (get end-block sub) block-height)
          (- (get end-block sub) block-height)
          u0)
    u0
  )
)

(define-public (create-plan (name (string-ascii 50)) (price uint) (duration-blocks uint) (features (string-ascii 200)))
  (let (
    (plan-id (var-get plan-nonce))
  )
    (map-set subscription-plans
      { plan-id: plan-id }
      {
        creator: tx-sender,
        name: name,
        price: price,
        duration-blocks: duration-blocks,
        features: features,
        active: true,
        subscriber-count: u0
      }
    )
    (var-set plan-nonce (+ plan-id u1))
    (ok plan-id)
  )
)

(define-public (subscribe (plan-id uint) (auto-renew bool))
  (let (
    (plan (unwrap! (get-plan plan-id) ERR-PLAN-NOT-FOUND))
    (existing (get-subscription tx-sender plan-id))
  )
    (asserts! (get active plan) ERR-PLAN-INACTIVE)
    (asserts! (not (is-subscribed tx-sender plan-id)) ERR-ALREADY-SUBSCRIBED)
    (map-set subscriptions
      { subscriber: tx-sender, plan-id: plan-id }
      {
        start-block: block-height,
        end-block: (+ block-height (get duration-blocks plan)),
        auto-renew: auto-renew,
        payments-made: u1
      }
    )
    (map-set subscription-plans
      { plan-id: plan-id }
      (merge plan { subscriber-count: (+ (get subscriber-count plan) u1) })
    )
    (ok true)
  )
)

(define-public (renew-subscription (plan-id uint))
  (let (
    (plan (unwrap! (get-plan plan-id) ERR-PLAN-NOT-FOUND))
    (sub (unwrap! (get-subscription tx-sender plan-id) ERR-NOT-SUBSCRIBED))
    (new-end (if (>= (get end-block sub) block-height)
                 (+ (get end-block sub) (get duration-blocks plan))
                 (+ block-height (get duration-blocks plan))))
  )
    (asserts! (get active plan) ERR-PLAN-INACTIVE)
    (map-set subscriptions
      { subscriber: tx-sender, plan-id: plan-id }
      (merge sub {
        end-block: new-end,
        payments-made: (+ (get payments-made sub) u1)
      })
    )
    (ok new-end)
  )
)

(define-public (cancel-subscription (plan-id uint))
  (let (
    (sub (unwrap! (get-subscription tx-sender plan-id) ERR-NOT-SUBSCRIBED))
  )
    (map-set subscriptions
      { subscriber: tx-sender, plan-id: plan-id }
      (merge sub { auto-renew: false })
    )
    (ok true)
  )
)

(define-public (deactivate-plan (plan-id uint))
  (let (
    (plan (unwrap! (get-plan plan-id) ERR-PLAN-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator plan)) ERR-NOT-AUTHORIZED)
    (map-set subscription-plans
      { plan-id: plan-id }
      (merge plan { active: false })
    )
    (ok true)
  )
)
