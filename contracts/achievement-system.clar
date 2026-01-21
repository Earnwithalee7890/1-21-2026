;; Achievement System
;; On-chain achievements and badges for dApps

(define-non-fungible-token achievement-badge uint)

(define-map achievements
  { achievement-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    points: uint,
    category: (string-ascii 20),
    tier: (string-ascii 10),
    max-claimers: uint,
    current-claimers: uint,
    criteria: (string-ascii 100),
    active: bool
  }
)

(define-map user-achievements
  { user: principal, achievement-id: uint }
  { 
    claimed-at: uint,
    badge-id: uint
  }
)

(define-map user-stats
  { user: principal }
  {
    total-points: uint,
    achievements-count: uint,
    rank: (string-ascii 20)
  }
)

(define-data-var achievement-nonce uint u0)
(define-data-var badge-nonce uint u0)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-CLAIMED (err u102))
(define-constant ERR-MAX-CLAIMERS-REACHED (err u103))
(define-constant ERR-ACHIEVEMENT-INACTIVE (err u104))

(define-read-only (get-achievement (achievement-id uint))
  (map-get? achievements { achievement-id: achievement-id })
)

(define-read-only (has-achievement (user principal) (achievement-id uint))
  (is-some (map-get? user-achievements { user: user, achievement-id: achievement-id }))
)

(define-read-only (get-user-stats (user principal))
  (default-to { total-points: u0, achievements-count: u0, rank: "Newcomer" }
    (map-get? user-stats { user: user }))
)

(define-read-only (calculate-rank (points uint))
  (if (>= points u10000) "Legend"
    (if (>= points u5000) "Master"
      (if (>= points u2500) "Expert"
        (if (>= points u1000) "Veteran"
          (if (>= points u500) "Skilled"
            (if (>= points u100) "Apprentice"
              "Newcomer"
            )
          )
        )
      )
    )
  )
)

(define-public (create-achievement (name (string-ascii 50)) (description (string-ascii 200)) (points uint) (category (string-ascii 20)) (tier (string-ascii 10)) (max-claimers uint) (criteria (string-ascii 100)))
  (let (
    (achievement-id (var-get achievement-nonce))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set achievements
      { achievement-id: achievement-id }
      {
        name: name,
        description: description,
        points: points,
        category: category,
        tier: tier,
        max-claimers: max-claimers,
        current-claimers: u0,
        criteria: criteria,
        active: true
      }
    )
    (var-set achievement-nonce (+ achievement-id u1))
    (ok achievement-id)
  )
)

(define-public (grant-achievement (user principal) (achievement-id uint))
  (let (
    (achievement (unwrap! (get-achievement achievement-id) ERR-ACHIEVEMENT-NOT-FOUND))
    (badge-id (var-get badge-nonce))
    (current-stats (get-user-stats user))
    (new-points (+ (get total-points current-stats) (get points achievement)))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get active achievement) ERR-ACHIEVEMENT-INACTIVE)
    (asserts! (not (has-achievement user achievement-id)) ERR-ALREADY-CLAIMED)
    (asserts! (< (get current-claimers achievement) (get max-claimers achievement)) ERR-MAX-CLAIMERS-REACHED)
    (try! (nft-mint? achievement-badge badge-id user))
    (map-set user-achievements
      { user: user, achievement-id: achievement-id }
      { claimed-at: block-height, badge-id: badge-id }
    )
    (map-set achievements
      { achievement-id: achievement-id }
      (merge achievement { current-claimers: (+ (get current-claimers achievement) u1) })
    )
    (map-set user-stats
      { user: user }
      {
        total-points: new-points,
        achievements-count: (+ (get achievements-count current-stats) u1),
        rank: (calculate-rank new-points)
      }
    )
    (var-set badge-nonce (+ badge-id u1))
    (ok badge-id)
  )
)

(define-public (deactivate-achievement (achievement-id uint))
  (let (
    (achievement (unwrap! (get-achievement achievement-id) ERR-ACHIEVEMENT-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set achievements
      { achievement-id: achievement-id }
      (merge achievement { active: false })
    )
    (ok true)
  )
)

(define-public (update-achievement-criteria (achievement-id uint) (new-criteria (string-ascii 100)))
  (let (
    (achievement (unwrap! (get-achievement achievement-id) ERR-ACHIEVEMENT-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set achievements
      { achievement-id: achievement-id }
      (merge achievement { criteria: new-criteria })
    )
    (ok true)
  )
)
