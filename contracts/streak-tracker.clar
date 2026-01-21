;; Streak Tracker - Clarity 4
;; Track daily check-in streaks

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u2400))
(define-constant err-too-early (err u2401))
(define-constant blocks-per-day u144)

(define-map streaks
    principal
    {
        current-streak: uint,
        longest-streak: uint,
        last-checkin: uint,
        total-checkins: uint
    }
)

(define-read-only (get-streak (user principal))
    (default-to {current-streak: u0, longest-streak: u0, last-checkin: u0, total-checkins: u0}
        (map-get? streaks user)
    )
)

(define-read-only (get-current-streak (user principal))
    (get current-streak (get-streak user))
)

(define-read-only (can-checkin (user principal))
    (let (
        (user-data (get-streak user))
        (blocks-since (- block-height (get last-checkin user-data)))
    )
        (>= blocks-since blocks-per-day)
    )
)

(define-public (checkin)
    (let (
        (user-data (get-streak tx-sender))
        (blocks-since (- block-height (get last-checkin user-data)))
        (streak-continues (and (>= blocks-since blocks-per-day) (<= blocks-since (* blocks-per-day u2))))
        (new-streak (if streak-continues
            (+ (get current-streak user-data) u1)
            u1
        ))
        (new-longest (if (> new-streak (get longest-streak user-data))
            new-streak
            (get longest-streak user-data)
        ))
    )
        (asserts! (>= blocks-since blocks-per-day) err-too-early)
        (ok (map-set streaks tx-sender {
            current-streak: new-streak,
            longest-streak: new-longest,
            last-checkin: block-height,
            total-checkins: (+ (get total-checkins user-data) u1)
        }))
    )
)

(define-public (reset-streak)
    (let (
        (user-data (get-streak tx-sender))
    )
        (ok (map-set streaks tx-sender
            (merge user-data {current-streak: u0})
        ))
    )
)
