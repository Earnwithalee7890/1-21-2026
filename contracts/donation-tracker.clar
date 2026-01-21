;; Donation Tracker
;; Track donations with goals

(define-data-var goal uint u1000000)
(define-data-var raised uint u0)
(define-data-var donor-count uint u0)
(define-data-var project-owner principal tx-sender)

(define-map donations principal uint)

(define-read-only (get-goal)
  (var-get goal)
)

(define-read-only (get-raised)
  (var-get raised)
)

(define-read-only (get-donor-count)
  (var-get donor-count)
)

(define-read-only (get-donation (donor principal))
  (default-to u0 (map-get? donations donor))
)

(define-read-only (is-goal-reached)
  (>= (var-get raised) (var-get goal))
)

(define-public (donate (amount uint))
  (let
    (
      (current (get-donation tx-sender))
    )
    (if (is-eq current u0)
      (var-set donor-count (+ (var-get donor-count) u1))
      true
    )
    (map-set donations tx-sender (+ current amount))
    (var-set raised (+ (var-get raised) amount))
    (ok (+ current amount))
  )
)

(define-public (set-goal (new-goal uint))
  (begin
    (asserts! (is-eq tx-sender (var-get project-owner)) (err u100))
    (var-set goal new-goal)
    (ok new-goal)
  )
)
