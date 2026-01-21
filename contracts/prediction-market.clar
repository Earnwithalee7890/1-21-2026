;; Prediction Market
;; Create and bet on prediction markets

(define-map markets
  { market-id: uint }
  {
    creator: principal,
    question: (string-ascii 200),
    end-block: uint,
    resolved: bool,
    outcome: (optional bool),
    yes-pool: uint,
    no-pool: uint,
    total-bets: uint
  }
)

(define-map positions
  { market-id: uint, user: principal }
  { yes-amount: uint, no-amount: uint, claimed: bool }
)

(define-data-var market-nonce uint u0)
(define-data-var resolution-fee uint u200)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-MARKET-NOT-FOUND (err u101))
(define-constant ERR-MARKET-CLOSED (err u102))
(define-constant ERR-NOT-RESOLVED (err u103))
(define-constant ERR-ALREADY-RESOLVED (err u104))
(define-constant ERR-ALREADY-CLAIMED (err u105))
(define-constant ERR-NO-POSITION (err u106))

(define-read-only (get-market (market-id uint))
  (map-get? markets { market-id: market-id })
)

(define-read-only (get-position (market-id uint) (user principal))
  (map-get? positions { market-id: market-id, user: user })
)

(define-read-only (get-odds (market-id uint))
  (let (
    (market (unwrap-panic (get-market market-id)))
    (total (+ (get yes-pool market) (get no-pool market)))
  )
    {
      yes-odds: (if (> total u0) (/ (* (get yes-pool market) u10000) total) u5000),
      no-odds: (if (> total u0) (/ (* (get no-pool market) u10000) total) u5000)
    }
  )
)

(define-read-only (is-market-open (market-id uint))
  (let (
    (market (unwrap-panic (get-market market-id)))
  )
    (and (not (get resolved market))
         (<= block-height (get end-block market)))
  )
)

(define-public (create-market (question (string-ascii 200)) (duration-blocks uint))
  (let (
    (market-id (var-get market-nonce))
  )
    (map-set markets
      { market-id: market-id }
      {
        creator: tx-sender,
        question: question,
        end-block: (+ block-height duration-blocks),
        resolved: false,
        outcome: none,
        yes-pool: u0,
        no-pool: u0,
        total-bets: u0
      }
    )
    (var-set market-nonce (+ market-id u1))
    (ok market-id)
  )
)

(define-public (bet-yes (market-id uint) (amount uint))
  (let (
    (market (unwrap! (get-market market-id) ERR-MARKET-NOT-FOUND))
    (pos (default-to { yes-amount: u0, no-amount: u0, claimed: false }
           (get-position market-id tx-sender)))
  )
    (asserts! (is-market-open market-id) ERR-MARKET-CLOSED)
    (map-set positions
      { market-id: market-id, user: tx-sender }
      (merge pos { yes-amount: (+ (get yes-amount pos) amount) })
    )
    (map-set markets
      { market-id: market-id }
      (merge market {
        yes-pool: (+ (get yes-pool market) amount),
        total-bets: (+ (get total-bets market) u1)
      })
    )
    (ok true)
  )
)

(define-public (bet-no (market-id uint) (amount uint))
  (let (
    (market (unwrap! (get-market market-id) ERR-MARKET-NOT-FOUND))
    (pos (default-to { yes-amount: u0, no-amount: u0, claimed: false }
           (get-position market-id tx-sender)))
  )
    (asserts! (is-market-open market-id) ERR-MARKET-CLOSED)
    (map-set positions
      { market-id: market-id, user: tx-sender }
      (merge pos { no-amount: (+ (get no-amount pos) amount) })
    )
    (map-set markets
      { market-id: market-id }
      (merge market {
        no-pool: (+ (get no-pool market) amount),
        total-bets: (+ (get total-bets market) u1)
      })
    )
    (ok true)
  )
)

(define-public (resolve-market (market-id uint) (outcome bool))
  (let (
    (market (unwrap! (get-market market-id) ERR-MARKET-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator market)) ERR-NOT-AUTHORIZED)
    (asserts! (> block-height (get end-block market)) ERR-MARKET-CLOSED)
    (asserts! (not (get resolved market)) ERR-ALREADY-RESOLVED)
    (map-set markets
      { market-id: market-id }
      (merge market { resolved: true, outcome: (some outcome) })
    )
    (ok true)
  )
)

(define-public (claim-winnings (market-id uint))
  (let (
    (market (unwrap! (get-market market-id) ERR-MARKET-NOT-FOUND))
    (pos (unwrap! (get-position market-id tx-sender) ERR-NO-POSITION))
    (outcome (unwrap! (get outcome market) ERR-NOT-RESOLVED))
    (total-pool (+ (get yes-pool market) (get no-pool market)))
    (winning-pool (if outcome (get yes-pool market) (get no-pool market)))
    (user-stake (if outcome (get yes-amount pos) (get no-amount pos)))
    (winnings (/ (* user-stake total-pool) winning-pool))
  )
    (asserts! (get resolved market) ERR-NOT-RESOLVED)
    (asserts! (not (get claimed pos)) ERR-ALREADY-CLAIMED)
    (asserts! (> user-stake u0) ERR-NO-POSITION)
    (map-set positions
      { market-id: market-id, user: tx-sender }
      (merge pos { claimed: true })
    )
    (ok winnings)
  )
)
