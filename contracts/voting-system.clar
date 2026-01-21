;; Voting System - Clarity 4
;; Create polls and vote

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1200))
(define-constant err-unauthorized (err u1201))
(define-constant err-already-voted (err u1202))
(define-constant err-poll-closed (err u1203))

(define-data-var poll-nonce uint u0)

(define-map polls
    uint
    {
        creator: principal,
        question: (string-utf8 200),
        yes-votes: uint,
        no-votes: uint,
        end-block: uint,
        open: bool
    }
)

(define-map votes
    {poll-id: uint, voter: principal}
    {vote: bool, voted-at: uint}
)

(define-read-only (get-poll (poll-id uint))
    (map-get? polls poll-id)
)

(define-read-only (has-voted (poll-id uint) (voter principal))
    (is-some (map-get? votes {poll-id: poll-id, voter: voter}))
)

(define-read-only (is-poll-open (poll-id uint))
    (match (get-poll poll-id)
        p (and (get open p) (<= stacks-block-height (get end-block p)))
        false
    )
)

(define-public (create-poll
    (question (string-utf8 200))
    (duration uint)
)
    (let (
        (poll-id (var-get poll-nonce))
    )
        (map-set polls poll-id {
            creator: tx-sender,
            question: question,
            yes-votes: u0,
            no-votes: u0,
            end-block: (+ stacks-block-height duration),
            open: true
        })
        (var-set poll-nonce (+ poll-id u1))
        (ok poll-id)
    )
)

(define-public (vote-yes (poll-id uint))
    (let (
        (poll (unwrap! (get-poll poll-id) err-not-found))
    )
        (asserts! (is-poll-open poll-id) err-poll-closed)
        (asserts! (not (has-voted poll-id tx-sender)) err-already-voted)
        (map-set votes {poll-id: poll-id, voter: tx-sender}
            {vote: true, voted-at: stacks-block-height}
        )
        (ok (map-set polls poll-id
            (merge poll {yes-votes: (+ (get yes-votes poll) u1)})
        ))
    )
)

(define-public (vote-no (poll-id uint))
    (let (
        (poll (unwrap! (get-poll poll-id) err-not-found))
    )
        (asserts! (is-poll-open poll-id) err-poll-closed)
        (asserts! (not (has-voted poll-id tx-sender)) err-already-voted)
        (map-set votes {poll-id: poll-id, voter: tx-sender}
            {vote: false, voted-at: stacks-block-height}
        )
        (ok (map-set polls poll-id
            (merge poll {no-votes: (+ (get no-votes poll) u1)})
        ))
    )
)

(define-public (close-poll (poll-id uint))
    (let (
        (poll (unwrap! (get-poll poll-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get creator poll)) err-unauthorized)
        (ok (map-set polls poll-id (merge poll {open: false})))
    )
)
