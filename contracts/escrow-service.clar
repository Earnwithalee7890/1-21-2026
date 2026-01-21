;; Escrow Service
;; Secure escrow for peer-to-peer transactions

(define-map escrows
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    arbiter: principal,
    amount: uint,
    status: (string-ascii 10),
    created-at: uint,
    deadline: uint,
    description: (string-ascii 200)
  }
)

(define-map disputes
  { escrow-id: uint }
  {
    raised-by: principal,
    reason: (string-ascii 200),
    resolved: bool,
    winner: (optional principal)
  }
)

(define-data-var escrow-nonce uint u0)
(define-data-var arbiter-fee uint u200)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ESCROW-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-ALREADY-DISPUTED (err u103))
(define-constant ERR-DEADLINE-PASSED (err u104))

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows { escrow-id: escrow-id })
)

(define-read-only (get-dispute (escrow-id uint))
  (map-get? disputes { escrow-id: escrow-id })
)

(define-public (create-escrow (seller principal) (arbiter principal) (amount uint) (deadline-blocks uint) (description (string-ascii 200)))
  (let (
    (escrow-id (var-get escrow-nonce))
  )
    (map-set escrows
      { escrow-id: escrow-id }
      {
        buyer: tx-sender,
        seller: seller,
        arbiter: arbiter,
        amount: amount,
        status: "funded",
        created-at: block-height,
        deadline: (+ block-height deadline-blocks),
        description: description
      }
    )
    (var-set escrow-nonce (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (release-funds (escrow-id uint))
  (let (
    (escrow (unwrap! (get-escrow escrow-id) ERR-ESCROW-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escrow) "funded") ERR-INVALID-STATUS)
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "released" })
    )
    (ok (get amount escrow))
  )
)

(define-public (refund (escrow-id uint))
  (let (
    (escrow (unwrap! (get-escrow escrow-id) ERR-ESCROW-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get seller escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escrow) "funded") ERR-INVALID-STATUS)
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "refunded" })
    )
    (ok (get amount escrow))
  )
)

(define-public (raise-dispute (escrow-id uint) (reason (string-ascii 200)))
  (let (
    (escrow (unwrap! (get-escrow escrow-id) ERR-ESCROW-NOT-FOUND))
  )
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escrow) "funded") ERR-INVALID-STATUS)
    (asserts! (is-none (get-dispute escrow-id)) ERR-ALREADY-DISPUTED)
    (map-set disputes
      { escrow-id: escrow-id }
      {
        raised-by: tx-sender,
        reason: reason,
        resolved: false,
        winner: none
      }
    )
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "disputed" })
    )
    (ok true)
  )
)

(define-public (resolve-dispute (escrow-id uint) (winner principal))
  (let (
    (escrow (unwrap! (get-escrow escrow-id) ERR-ESCROW-NOT-FOUND))
    (dispute (unwrap! (get-dispute escrow-id) ERR-ESCROW-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get arbiter escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escrow) "disputed") ERR-INVALID-STATUS)
    (map-set disputes
      { escrow-id: escrow-id }
      (merge dispute { resolved: true, winner: (some winner) })
    )
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "resolved" })
    )
    (ok (get amount escrow))
  )
)
