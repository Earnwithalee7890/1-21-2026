;; Token Launchpad
;; Create and launch new tokens with fair distribution

(define-map launches
  { launch-id: uint }
  {
    creator: principal,
    token-name: (string-ascii 50),
    token-symbol: (string-ascii 10),
    total-supply: uint,
    tokens-for-sale: uint,
    tokens-sold: uint,
    price-per-token: uint,
    start-block: uint,
    end-block: uint,
    min-contribution: uint,
    max-contribution: uint,
    soft-cap: uint,
    hard-cap: uint,
    raised: uint,
    finalized: bool
  }
)

(define-map participant-allocations
  { launch-id: uint, participant: principal }
  { contribution: uint, tokens-claimed: bool }
)

(define-data-var launch-nonce uint u0)
(define-data-var platform-fee uint u300)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LAUNCH-NOT-FOUND (err u101))
(define-constant ERR-NOT-STARTED (err u102))
(define-constant ERR-ALREADY-ENDED (err u103))
(define-constant ERR-BELOW-MIN (err u104))
(define-constant ERR-ABOVE-MAX (err u105))
(define-constant ERR-HARD-CAP-REACHED (err u106))
(define-constant ERR-NOT-FINALIZED (err u107))
(define-constant ERR-ALREADY-CLAIMED (err u108))

(define-read-only (get-launch (launch-id uint))
  (map-get? launches { launch-id: launch-id })
)

(define-read-only (get-allocation (launch-id uint) (participant principal))
  (map-get? participant-allocations { launch-id: launch-id, participant: participant })
)

(define-read-only (is-launch-active (launch-id uint))
  (let (
    (launch (unwrap-panic (get-launch launch-id)))
  )
    (and (>= block-height (get start-block launch))
         (<= block-height (get end-block launch))
         (< (get raised launch) (get hard-cap launch)))
  )
)

(define-read-only (calculate-tokens (launch-id uint) (contribution uint))
  (let (
    (launch (unwrap-panic (get-launch launch-id)))
  )
    (/ (* contribution u1000000) (get price-per-token launch))
  )
)

(define-public (create-launch 
  (token-name (string-ascii 50)) 
  (token-symbol (string-ascii 10)) 
  (total-supply uint) 
  (tokens-for-sale uint) 
  (price uint) 
  (start-blocks uint) 
  (duration-blocks uint)
  (min-contrib uint)
  (max-contrib uint)
  (soft-cap uint)
  (hard-cap uint))
  (let (
    (launch-id (var-get launch-nonce))
  )
    (map-set launches
      { launch-id: launch-id }
      {
        creator: tx-sender,
        token-name: token-name,
        token-symbol: token-symbol,
        total-supply: total-supply,
        tokens-for-sale: tokens-for-sale,
        tokens-sold: u0,
        price-per-token: price,
        start-block: (+ block-height start-blocks),
        end-block: (+ block-height start-blocks duration-blocks),
        min-contribution: min-contrib,
        max-contribution: max-contrib,
        soft-cap: soft-cap,
        hard-cap: hard-cap,
        raised: u0,
        finalized: false
      }
    )
    (var-set launch-nonce (+ launch-id u1))
    (ok launch-id)
  )
)

(define-public (participate (launch-id uint) (amount uint))
  (let (
    (launch (unwrap! (get-launch launch-id) ERR-LAUNCH-NOT-FOUND))
    (existing (get-allocation launch-id tx-sender))
    (prev-contrib (default-to u0 (get contribution existing)))
    (total-contrib (+ prev-contrib amount))
    (tokens (calculate-tokens launch-id amount))
  )
    (asserts! (is-launch-active launch-id) ERR-ALREADY-ENDED)
    (asserts! (>= total-contrib (get min-contribution launch)) ERR-BELOW-MIN)
    (asserts! (<= total-contrib (get max-contribution launch)) ERR-ABOVE-MAX)
    (asserts! (<= (+ (get raised launch) amount) (get hard-cap launch)) ERR-HARD-CAP-REACHED)
    (map-set participant-allocations
      { launch-id: launch-id, participant: tx-sender }
      { contribution: total-contrib, tokens-claimed: false }
    )
    (map-set launches
      { launch-id: launch-id }
      (merge launch {
        raised: (+ (get raised launch) amount),
        tokens-sold: (+ (get tokens-sold launch) tokens)
      })
    )
    (ok tokens)
  )
)

(define-public (finalize-launch (launch-id uint))
  (let (
    (launch (unwrap! (get-launch launch-id) ERR-LAUNCH-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator launch)) ERR-NOT-AUTHORIZED)
    (asserts! (> block-height (get end-block launch)) ERR-NOT-STARTED)
    (map-set launches
      { launch-id: launch-id }
      (merge launch { finalized: true })
    )
    (ok (get raised launch))
  )
)

(define-public (claim-tokens (launch-id uint))
  (let (
    (launch (unwrap! (get-launch launch-id) ERR-LAUNCH-NOT-FOUND))
    (alloc (unwrap! (get-allocation launch-id tx-sender) ERR-NOT-AUTHORIZED))
    (tokens (calculate-tokens launch-id (get contribution alloc)))
  )
    (asserts! (get finalized launch) ERR-NOT-FINALIZED)
    (asserts! (not (get tokens-claimed alloc)) ERR-ALREADY-CLAIMED)
    (map-set participant-allocations
      { launch-id: launch-id, participant: tx-sender }
      (merge alloc { tokens-claimed: true })
    )
    (ok tokens)
  )
)
