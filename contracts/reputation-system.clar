;; Reputation System
;; On-chain reputation with endorsements and scores

(define-map user-reputation
  { user: principal }
  {
    total-score: uint,
    positive-endorsements: uint,
    negative-endorsements: uint,
    level: uint,
    joined-at: uint
  }
)

(define-map endorsements
  { endorser: principal, endorsed: principal }
  { 
    score: int,
    category: (string-ascii 20),
    timestamp: uint,
    comment: (string-ascii 100)
  }
)

(define-map level-thresholds
  { level: uint }
  { min-score: uint, title: (string-ascii 20) }
)

(define-data-var endorsement-cooldown uint u144)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-NOT-FOUND (err u101))
(define-constant ERR-SELF-ENDORSEMENT (err u102))
(define-constant ERR-COOLDOWN-ACTIVE (err u103))
(define-constant ERR-ALREADY-ENDORSED (err u104))

(define-read-only (get-reputation (user principal))
  (map-get? user-reputation { user: user })
)

(define-read-only (get-endorsement (endorser principal) (endorsed principal))
  (map-get? endorsements { endorser: endorser, endorsed: endorsed })
)

(define-read-only (get-level-info (level uint))
  (map-get? level-thresholds { level: level })
)

(define-read-only (calculate-level (score uint))
  (if (>= score u10000) u10
    (if (>= score u5000) u9
      (if (>= score u2500) u8
        (if (>= score u1000) u7
          (if (>= score u500) u6
            (if (>= score u250) u5
              (if (>= score u100) u4
                (if (>= score u50) u3
                  (if (>= score u25) u2
                    u1
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(define-public (register)
  (begin
    (map-set user-reputation
      { user: tx-sender }
      {
        total-score: u0,
        positive-endorsements: u0,
        negative-endorsements: u0,
        level: u1,
        joined-at: block-height
      }
    )
    (ok true)
  )
)

(define-public (endorse-positive (user principal) (category (string-ascii 20)) (comment (string-ascii 100)))
  (let (
    (target-rep (unwrap! (get-reputation user) ERR-USER-NOT-FOUND))
    (endorser-rep (unwrap! (get-reputation tx-sender) ERR-USER-NOT-FOUND))
    (new-score (+ (get total-score target-rep) (+ u10 (get level endorser-rep))))
  )
    (asserts! (not (is-eq tx-sender user)) ERR-SELF-ENDORSEMENT)
    (asserts! (is-none (get-endorsement tx-sender user)) ERR-ALREADY-ENDORSED)
    (map-set endorsements
      { endorser: tx-sender, endorsed: user }
      {
        score: 1,
        category: category,
        timestamp: block-height,
        comment: comment
      }
    )
    (map-set user-reputation
      { user: user }
      (merge target-rep {
        total-score: new-score,
        positive-endorsements: (+ (get positive-endorsements target-rep) u1),
        level: (calculate-level new-score)
      })
    )
    (ok new-score)
  )
)

(define-public (endorse-negative (user principal) (category (string-ascii 20)) (comment (string-ascii 100)))
  (let (
    (target-rep (unwrap! (get-reputation user) ERR-USER-NOT-FOUND))
    (penalty (min u5 (get total-score target-rep)))
    (new-score (- (get total-score target-rep) penalty))
  )
    (asserts! (not (is-eq tx-sender user)) ERR-SELF-ENDORSEMENT)
    (asserts! (is-none (get-endorsement tx-sender user)) ERR-ALREADY-ENDORSED)
    (map-set endorsements
      { endorser: tx-sender, endorsed: user }
      {
        score: -1,
        category: category,
        timestamp: block-height,
        comment: comment
      }
    )
    (map-set user-reputation
      { user: user }
      (merge target-rep {
        total-score: new-score,
        negative-endorsements: (+ (get negative-endorsements target-rep) u1),
        level: (calculate-level new-score)
      })
    )
    (ok new-score)
  )
)

(define-public (set-level-threshold (level uint) (min-score uint) (title (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set level-thresholds
      { level: level }
      { min-score: min-score, title: title }
    )
    (ok true)
  )
)
