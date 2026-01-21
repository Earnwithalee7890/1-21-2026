;; DAO Governance System
;; Create and vote on proposals with token-weighted voting

(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    execution-delay: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { amount: uint, support: bool }
)

(define-map voting-power
  { voter: principal }
  { power: uint, delegated-to: (optional principal) }
)

(define-data-var proposal-count uint u0)
(define-data-var quorum-threshold uint u1000000)
(define-data-var proposal-threshold uint u100000)
(define-data-var voting-period uint u1440)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-VOTING-CLOSED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-INSUFFICIENT-POWER (err u104))
(define-constant ERR-NOT-EXECUTABLE (err u105))

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voting-power (voter principal))
  (default-to { power: u0, delegated-to: none }
    (map-get? voting-power { voter: voter }))
)

(define-read-only (is-voting-open (proposal-id uint))
  (let (
    (proposal (unwrap-panic (get-proposal proposal-id)))
  )
    (and (>= block-height (get start-block proposal))
         (<= block-height (get end-block proposal)))
  )
)

(define-read-only (has-passed (proposal-id uint))
  (let (
    (proposal (unwrap-panic (get-proposal proposal-id)))
  )
    (and (> (get votes-for proposal) (get votes-against proposal))
         (>= (+ (get votes-for proposal) (get votes-against proposal)) (var-get quorum-threshold)))
  )
)

(define-public (register-voting-power (amount uint))
  (let (
    (current-power (get-voting-power tx-sender))
  )
    (map-set voting-power
      { voter: tx-sender }
      { power: (+ (get power current-power) amount), delegated-to: none }
    )
    (ok true)
  )
)

(define-public (delegate-power (delegate principal))
  (let (
    (current-power (get-voting-power tx-sender))
  )
    (map-set voting-power
      { voter: tx-sender }
      (merge current-power { delegated-to: (some delegate) })
    )
    (ok true)
  )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)))
  (let (
    (proposal-id (var-get proposal-count))
    (proposer-power (get power (get-voting-power tx-sender)))
  )
    (asserts! (>= proposer-power (var-get proposal-threshold)) ERR-INSUFFICIENT-POWER)
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        start-block: (+ block-height u1),
        end-block: (+ block-height (var-get voting-period)),
        votes-for: u0,
        votes-against: u0,
        executed: false,
        execution-delay: u144
      }
    )
    (var-set proposal-count (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (cast-vote (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (voter-power (get power (get-voting-power tx-sender)))
  )
    (asserts! (is-voting-open proposal-id) ERR-VOTING-CLOSED)
    (asserts! (is-none (get-vote proposal-id tx-sender)) ERR-ALREADY-VOTED)
    (asserts! (> voter-power u0) ERR-INSUFFICIENT-POWER)
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { amount: voter-power, support: support }
    )
    (map-set proposals
      { proposal-id: proposal-id }
      (if support
        (merge proposal { votes-for: (+ (get votes-for proposal) voter-power) })
        (merge proposal { votes-against: (+ (get votes-against proposal) voter-power) })
      )
    )
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
  )
    (asserts! (> block-height (+ (get end-block proposal) (get execution-delay proposal))) ERR-NOT-EXECUTABLE)
    (asserts! (has-passed proposal-id) ERR-NOT-EXECUTABLE)
    (asserts! (not (get executed proposal)) ERR-NOT-EXECUTABLE)
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    (ok true)
  )
)
