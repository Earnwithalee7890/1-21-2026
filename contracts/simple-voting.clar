;; Simple Voting
;; Vote yes or no on a proposal

(define-data-var proposal-title (string-ascii 100) "Default Proposal")
(define-data-var yes-votes uint u0)
(define-data-var no-votes uint u0)
(define-data-var voting-open bool true)
(define-data-var admin principal tx-sender)

(define-map voters principal bool)

(define-read-only (get-proposal)
  (var-get proposal-title)
)

(define-read-only (get-yes-votes)
  (var-get yes-votes)
)

(define-read-only (get-no-votes)
  (var-get no-votes)
)

(define-read-only (is-voting-open)
  (var-get voting-open)
)

(define-read-only (has-voted (voter principal))
  (default-to false (map-get? voters voter))
)

(define-public (vote-yes)
  (begin
    (asserts! (var-get voting-open) (err u101))
    (asserts! (not (has-voted tx-sender)) (err u102))
    (map-set voters tx-sender true)
    (var-set yes-votes (+ (var-get yes-votes) u1))
    (ok true)
  )
)

(define-public (vote-no)
  (begin
    (asserts! (var-get voting-open) (err u101))
    (asserts! (not (has-voted tx-sender)) (err u102))
    (map-set voters tx-sender true)
    (var-set no-votes (+ (var-get no-votes) u1))
    (ok true)
  )
)

(define-public (close-voting)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u100))
    (var-set voting-open false)
    (ok true)
  )
)

(define-public (set-proposal (title (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u100))
    (var-set proposal-title title)
    (var-set yes-votes u0)
    (var-set no-votes u0)
    (var-set voting-open true)
    (ok true)
  )
)
