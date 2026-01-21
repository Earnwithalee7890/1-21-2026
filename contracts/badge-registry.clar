;; Badge Registry - Clarity 4
;; Issue and verify badges

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u2300))
(define-constant err-unauthorized (err u2301))
(define-constant err-already-has-badge (err u2302))

(define-data-var badge-type-nonce uint u0)
(define-data-var badge-nonce uint u0)

(define-map badge-types
    uint
    {
        creator: principal,
        name: (string-utf8 50),
        description: (string-utf8 200),
        max-supply: uint,
        issued-count: uint
    }
)

(define-map badges
    uint
    {
        badge-type-id: uint,
        holder: principal,
        issued-at: uint,
        issuer: principal
    }
)

(define-map holder-badges
    {badge-type-id: uint, holder: principal}
    {badge-id: uint}
)

(define-read-only (get-badge-type (type-id uint))
    (map-get? badge-types type-id)
)

(define-read-only (get-badge (badge-id uint))
    (map-get? badges badge-id)
)

(define-read-only (has-badge (type-id uint) (holder principal))
    (is-some (map-get? holder-badges {badge-type-id: type-id, holder: holder}))
)

(define-public (create-badge-type
    (name (string-utf8 50))
    (description (string-utf8 200))
    (max-supply uint)
)
    (let (
        (type-id (var-get badge-type-nonce))
    )
        (map-set badge-types type-id {
            creator: tx-sender,
            name: name,
            description: description,
            max-supply: max-supply,
            issued-count: u0
        })
        (var-set badge-type-nonce (+ type-id u1))
        (ok type-id)
    )
)

(define-public (issue-badge (type-id uint) (recipient principal))
    (let (
        (badge-type (unwrap! (get-badge-type type-id) err-not-found))
        (badge-id (var-get badge-nonce))
    )
        (asserts! (is-eq tx-sender (get creator badge-type)) err-unauthorized)
        (asserts! (not (has-badge type-id recipient)) err-already-has-badge)
        (asserts! (< (get issued-count badge-type) (get max-supply badge-type)) err-unauthorized)
        (map-set badges badge-id {
            badge-type-id: type-id,
            holder: recipient,
            issued-at: stacks-block-height,
            issuer: tx-sender
        })
        (map-set holder-badges {badge-type-id: type-id, holder: recipient}
            {badge-id: badge-id}
        )
        (map-set badge-types type-id
            (merge badge-type {issued-count: (+ (get issued-count badge-type) u1)})
        )
        (var-set badge-nonce (+ badge-id u1))
        (ok badge-id)
    )
)
