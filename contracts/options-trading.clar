;; Options Trading Protocol
;; Create and trade call/put options on Stacks

(define-map options
  { option-id: uint }
  {
    creator: principal,
    option-type: (string-ascii 4),
    underlying-asset: principal,
    strike-price: uint,
    premium: uint,
    expiry-block: uint,
    collateral: uint,
    exercised: bool,
    holder: (optional principal)
  }
)

(define-data-var option-nonce uint u0)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-OPTION-NOT-FOUND (err u101))
(define-constant ERR-OPTION-EXPIRED (err u102))
(define-constant ERR-NOT-HOLDER (err u103))
(define-constant ERR-ALREADY-EXERCISED (err u104))
(define-constant ERR-OPTION-NOT-EXPIRED (err u105))
(define-constant ERR-ALREADY-PURCHASED (err u106))

(define-read-only (get-option (option-id uint))
  (map-get? options { option-id: option-id })
)

(define-read-only (is-option-expired (option-id uint))
  (let (
    (option (unwrap-panic (get-option option-id)))
  )
    (> block-height (get expiry-block option))
  )
)

(define-read-only (get-total-options)
  (var-get option-nonce)
)

(define-public (create-call-option (underlying principal) (strike uint) (premium uint) (expiry-blocks uint) (collateral uint))
  (let (
    (option-id (var-get option-nonce))
  )
    (map-set options
      { option-id: option-id }
      {
        creator: tx-sender,
        option-type: "CALL",
        underlying-asset: underlying,
        strike-price: strike,
        premium: premium,
        expiry-block: (+ block-height expiry-blocks),
        collateral: collateral,
        exercised: false,
        holder: none
      }
    )
    (var-set option-nonce (+ option-id u1))
    (ok option-id)
  )
)

(define-public (create-put-option (underlying principal) (strike uint) (premium uint) (expiry-blocks uint) (collateral uint))
  (let (
    (option-id (var-get option-nonce))
  )
    (map-set options
      { option-id: option-id }
      {
        creator: tx-sender,
        option-type: "PUT",
        underlying-asset: underlying,
        strike-price: strike,
        premium: premium,
        expiry-block: (+ block-height expiry-blocks),
        collateral: collateral,
        exercised: false,
        holder: none
      }
    )
    (var-set option-nonce (+ option-id u1))
    (ok option-id)
  )
)

(define-public (buy-option (option-id uint))
  (let (
    (option (unwrap! (get-option option-id) ERR-OPTION-NOT-FOUND))
  )
    (asserts! (is-none (get holder option)) ERR-ALREADY-PURCHASED)
    (asserts! (not (is-option-expired option-id)) ERR-OPTION-EXPIRED)
    (map-set options
      { option-id: option-id }
      (merge option { holder: (some tx-sender) })
    )
    (ok true)
  )
)

(define-public (exercise-option (option-id uint))
  (let (
    (option (unwrap! (get-option option-id) ERR-OPTION-NOT-FOUND))
  )
    (asserts! (is-eq (some tx-sender) (get holder option)) ERR-NOT-HOLDER)
    (asserts! (not (get exercised option)) ERR-ALREADY-EXERCISED)
    (asserts! (not (is-option-expired option-id)) ERR-OPTION-EXPIRED)
    (map-set options
      { option-id: option-id }
      (merge option { exercised: true })
    )
    (ok (get collateral option))
  )
)

(define-public (reclaim-collateral (option-id uint))
  (let (
    (option (unwrap! (get-option option-id) ERR-OPTION-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator option)) ERR-NOT-AUTHORIZED)
    (asserts! (is-option-expired option-id) ERR-OPTION-NOT-EXPIRED)
    (asserts! (not (get exercised option)) ERR-ALREADY-EXERCISED)
    (map-delete options { option-id: option-id })
    (ok (get collateral option))
  )
)
