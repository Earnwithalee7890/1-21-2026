;; Lottery System
;; Verifiably random lottery with prize pools

(define-map lotteries
  { lottery-id: uint }
  {
    creator: principal,
    ticket-price: uint,
    max-tickets: uint,
    tickets-sold: uint,
    prize-pool: uint,
    end-block: uint,
    status: (string-ascii 10),
    winner: (optional principal),
    random-seed: (optional (buff 32))
  }
)

(define-map tickets
  { lottery-id: uint, ticket-number: uint }
  { holder: principal }
)

(define-map user-tickets
  { lottery-id: uint, user: principal }
  { ticket-numbers: (list 100 uint) }
)

(define-data-var lottery-nonce uint u0)
(define-data-var house-fee uint u500)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LOTTERY-NOT-FOUND (err u101))
(define-constant ERR-LOTTERY-ENDED (err u102))
(define-constant ERR-SOLD-OUT (err u103))
(define-constant ERR-NOT-ENDED (err u104))
(define-constant ERR-ALREADY-DRAWN (err u105))

(define-read-only (get-lottery (lottery-id uint))
  (map-get? lotteries { lottery-id: lottery-id })
)

(define-read-only (get-ticket-holder (lottery-id uint) (ticket-number uint))
  (map-get? tickets { lottery-id: lottery-id, ticket-number: ticket-number })
)

(define-read-only (get-user-ticket-count (lottery-id uint) (user principal))
  (match (map-get? user-tickets { lottery-id: lottery-id, user: user })
    data (len (get ticket-numbers data))
    u0
  )
)

(define-read-only (is-lottery-active (lottery-id uint))
  (let (
    (lottery (unwrap-panic (get-lottery lottery-id)))
  )
    (and (is-eq (get status lottery) "active")
         (<= block-height (get end-block lottery))
         (< (get tickets-sold lottery) (get max-tickets lottery)))
  )
)

(define-public (create-lottery (ticket-price uint) (max-tickets uint) (duration-blocks uint))
  (let (
    (lottery-id (var-get lottery-nonce))
  )
    (map-set lotteries
      { lottery-id: lottery-id }
      {
        creator: tx-sender,
        ticket-price: ticket-price,
        max-tickets: max-tickets,
        tickets-sold: u0,
        prize-pool: u0,
        end-block: (+ block-height duration-blocks),
        status: "active",
        winner: none,
        random-seed: none
      }
    )
    (var-set lottery-nonce (+ lottery-id u1))
    (ok lottery-id)
  )
)

(define-public (buy-tickets (lottery-id uint) (quantity uint))
  (let (
    (lottery (unwrap! (get-lottery lottery-id) ERR-LOTTERY-NOT-FOUND))
    (start-ticket (get tickets-sold lottery))
  )
    (asserts! (is-lottery-active lottery-id) ERR-LOTTERY-ENDED)
    (asserts! (<= (+ (get tickets-sold lottery) quantity) (get max-tickets lottery)) ERR-SOLD-OUT)
    (fold assign-ticket 
      (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
      { lottery-id: lottery-id, start: start-ticket, count: u0, max: quantity }
    )
    (map-set lotteries
      { lottery-id: lottery-id }
      (merge lottery {
        tickets-sold: (+ (get tickets-sold lottery) quantity),
        prize-pool: (+ (get prize-pool lottery) (* quantity (get ticket-price lottery)))
      })
    )
    (ok quantity)
  )
)

(define-private (assign-ticket (index uint) (state { lottery-id: uint, start: uint, count: uint, max: uint }))
  (if (< (get count state) (get max state))
    (begin
      (map-set tickets
        { lottery-id: (get lottery-id state), ticket-number: (+ (get start state) (get count state)) }
        { holder: tx-sender }
      )
      { lottery-id: (get lottery-id state), start: (get start state), count: (+ (get count state) u1), max: (get max state) }
    )
    state
  )
)

(define-public (draw-winner (lottery-id uint))
  (let (
    (lottery (unwrap! (get-lottery lottery-id) ERR-LOTTERY-NOT-FOUND))
    (seed (unwrap-panic (get-block-info? id-header-hash (- block-height u1))))
    (winner-index (mod (buff-to-uint-be (unwrap-panic (slice? seed u0 u8))) (get tickets-sold lottery)))
    (winner-ticket (unwrap-panic (get-ticket-holder lottery-id winner-index)))
  )
    (asserts! (> block-height (get end-block lottery)) ERR-NOT-ENDED)
    (asserts! (is-none (get winner lottery)) ERR-ALREADY-DRAWN)
    (map-set lotteries
      { lottery-id: lottery-id }
      (merge lottery {
        status: "completed",
        winner: (some (get holder winner-ticket)),
        random-seed: (some seed)
      })
    )
    (ok (get holder winner-ticket))
  )
)

(define-public (claim-prize (lottery-id uint))
  (let (
    (lottery (unwrap! (get-lottery lottery-id) ERR-LOTTERY-NOT-FOUND))
    (fee (/ (* (get prize-pool lottery) (var-get house-fee)) u10000))
    (prize (- (get prize-pool lottery) fee))
  )
    (asserts! (is-eq (some tx-sender) (get winner lottery)) ERR-NOT-AUTHORIZED)
    (ok prize)
  )
)
