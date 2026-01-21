;; Bounty Platform
;; Create and claim bounties for tasks and bug reports

(define-map bounties
  { bounty-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    reward: uint,
    deadline: uint,
    status: (string-ascii 10),
    winner: (optional principal),
    category: (string-ascii 20)
  }
)

(define-map submissions
  { bounty-id: uint, submitter: principal }
  {
    content: (string-ascii 500),
    submitted-at: uint,
    approved: bool
  }
)

(define-map bounty-submissions-list
  { bounty-id: uint }
  { submitters: (list 50 principal) }
)

(define-data-var bounty-nonce uint u0)
(define-data-var platform-fee uint u250)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-BOUNTY-NOT-FOUND (err u101))
(define-constant ERR-BOUNTY-CLOSED (err u102))
(define-constant ERR-ALREADY-SUBMITTED (err u103))
(define-constant ERR-NO-SUBMISSION (err u104))
(define-constant ERR-DEADLINE-PASSED (err u105))

(define-read-only (get-bounty (bounty-id uint))
  (map-get? bounties { bounty-id: bounty-id })
)

(define-read-only (get-submission (bounty-id uint) (submitter principal))
  (map-get? submissions { bounty-id: bounty-id, submitter: submitter })
)

(define-read-only (is-bounty-open (bounty-id uint))
  (let (
    (bounty (unwrap-panic (get-bounty bounty-id)))
  )
    (and (is-eq (get status bounty) "open")
         (<= block-height (get deadline bounty)))
  )
)

(define-public (create-bounty (title (string-ascii 100)) (description (string-ascii 500)) (reward uint) (deadline-blocks uint) (category (string-ascii 20)))
  (let (
    (bounty-id (var-get bounty-nonce))
  )
    (map-set bounties
      { bounty-id: bounty-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        reward: reward,
        deadline: (+ block-height deadline-blocks),
        status: "open",
        winner: none,
        category: category
      }
    )
    (var-set bounty-nonce (+ bounty-id u1))
    (ok bounty-id)
  )
)

(define-public (submit-work (bounty-id uint) (content (string-ascii 500)))
  (let (
    (bounty (unwrap! (get-bounty bounty-id) ERR-BOUNTY-NOT-FOUND))
  )
    (asserts! (is-bounty-open bounty-id) ERR-BOUNTY-CLOSED)
    (asserts! (is-none (get-submission bounty-id tx-sender)) ERR-ALREADY-SUBMITTED)
    (map-set submissions
      { bounty-id: bounty-id, submitter: tx-sender }
      {
        content: content,
        submitted-at: block-height,
        approved: false
      }
    )
    (ok true)
  )
)

(define-public (approve-submission (bounty-id uint) (winner principal))
  (let (
    (bounty (unwrap! (get-bounty bounty-id) ERR-BOUNTY-NOT-FOUND))
    (submission (unwrap! (get-submission bounty-id winner) ERR-NO-SUBMISSION))
  )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR-NOT-AUTHORIZED)
    (map-set submissions
      { bounty-id: bounty-id, submitter: winner }
      (merge submission { approved: true })
    )
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty {
        status: "completed",
        winner: (some winner)
      })
    )
    (ok (get reward bounty))
  )
)

(define-public (cancel-bounty (bounty-id uint))
  (let (
    (bounty (unwrap! (get-bounty bounty-id) ERR-BOUNTY-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status bounty) "open") ERR-BOUNTY-CLOSED)
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "cancelled" })
    )
    (ok (get reward bounty))
  )
)

(define-public (extend-deadline (bounty-id uint) (additional-blocks uint))
  (let (
    (bounty (unwrap! (get-bounty bounty-id) ERR-BOUNTY-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR-NOT-AUTHORIZED)
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { deadline: (+ (get deadline bounty) additional-blocks) })
    )
    (ok (+ (get deadline bounty) additional-blocks))
  )
)
