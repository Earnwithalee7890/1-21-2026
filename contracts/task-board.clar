;; Task Board - Clarity 4
;; Create and complete tasks for rewards

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1600))
(define-constant err-unauthorized (err u1601))
(define-constant err-already-completed (err u1602))

(define-data-var task-nonce uint u0)

(define-map tasks
    uint
    {
        creator: principal,
        title: (string-utf8 100),
        description: (string-utf8 300),
        reward: uint,
        assignee: (optional principal),
        completed: bool,
        created-at: uint
    }
)

(define-read-only (get-task (task-id uint))
    (map-get? tasks task-id)
)

(define-read-only (get-task-count)
    (var-get task-nonce)
)

(define-public (create-task
    (title (string-utf8 100))
    (description (string-utf8 300))
    (reward uint)
)
    (let (
        (task-id (var-get task-nonce))
    )
        (try! (stx-transfer? reward tx-sender (as-contract tx-sender)))
        (map-set tasks task-id {
            creator: tx-sender,
            title: title,
            description: description,
            reward: reward,
            assignee: none,
            completed: false,
            created-at: stacks-block-height
        })
        (var-set task-nonce (+ task-id u1))
        (ok task-id)
    )
)

(define-public (claim-task (task-id uint))
    (let (
        (task (unwrap! (get-task task-id) err-not-found))
    )
        (asserts! (is-none (get assignee task)) err-already-completed)
        (ok (map-set tasks task-id
            (merge task {assignee: (some tx-sender)})
        ))
    )
)

(define-public (approve-completion (task-id uint))
    (let (
        (task (unwrap! (get-task task-id) err-not-found))
        (worker (unwrap! (get assignee task) err-not-found))
    )
        (asserts! (is-eq tx-sender (get creator task)) err-unauthorized)
        (asserts! (not (get completed task)) err-already-completed)
        (map-set tasks task-id (merge task {completed: true}))
        (try! (as-contract (stx-transfer? (get reward task) tx-sender worker)))
        (ok true)
    )
)

(define-public (cancel-task (task-id uint))
    (let (
        (task (unwrap! (get-task task-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get creator task)) err-unauthorized)
        (asserts! (is-none (get assignee task)) err-already-completed)
        (try! (as-contract (stx-transfer? (get reward task) tx-sender (get creator task))))
        (ok (map-delete tasks task-id))
    )
)
