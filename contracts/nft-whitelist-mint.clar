;; NFT Collection with Whitelist
;; Minting with whitelist and public sale phases

(define-non-fungible-token collection-nft uint)

(define-map whitelist
  { address: principal }
  { spots: uint, minted: uint }
)

(define-data-var token-counter uint u0)
(define-data-var max-supply uint u10000)
(define-data-var whitelist-price uint u50000000)
(define-data-var public-price uint u75000000)
(define-data-var whitelist-active bool true)
(define-data-var public-sale-active bool false)
(define-data-var max-per-wallet uint u5)
(define-data-var base-uri (string-ascii 100) "ipfs://QmXxXxX/")

(define-map minted-count
  { minter: principal }
  { count: uint }
)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-SOLD-OUT (err u101))
(define-constant ERR-NOT-WHITELISTED (err u102))
(define-constant ERR-MAX-MINTED (err u103))
(define-constant ERR-SALE-NOT-ACTIVE (err u104))
(define-constant ERR-WRONG-PRICE (err u105))

(define-read-only (get-whitelist-info (address principal))
  (map-get? whitelist { address: address })
)

(define-read-only (get-minted-count (minter principal))
  (default-to { count: u0 }
    (map-get? minted-count { minter: minter }))
)

(define-read-only (get-total-supply)
  (var-get token-counter)
)

(define-read-only (get-token-uri (token-id uint))
  (ok (some (var-get base-uri)))
)

(define-read-only (get-owner (token-id uint))
  (nft-get-owner? collection-nft token-id)
)

(define-public (add-to-whitelist (addresses (list 100 principal)) (spots-per-address uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map set-whitelist-entry addresses spots-per-address))
  )
)

(define-private (set-whitelist-entry (address principal) (spots uint))
  (map-set whitelist
    { address: address }
    { spots: spots, minted: u0 }
  )
)

(define-public (whitelist-mint (amount uint))
  (let (
    (wl-info (unwrap! (get-whitelist-info tx-sender) ERR-NOT-WHITELISTED))
    (current-supply (var-get token-counter))
  )
    (asserts! (var-get whitelist-active) ERR-SALE-NOT-ACTIVE)
    (asserts! (<= (+ current-supply amount) (var-get max-supply)) ERR-SOLD-OUT)
    (asserts! (<= (+ (get minted wl-info) amount) (get spots wl-info)) ERR-MAX-MINTED)
    (map-set whitelist
      { address: tx-sender }
      { spots: (get spots wl-info), minted: (+ (get minted wl-info) amount) }
    )
    (mint-batch amount)
  )
)

(define-public (public-mint (amount uint))
  (let (
    (current-supply (var-get token-counter))
    (minter-info (get-minted-count tx-sender))
  )
    (asserts! (var-get public-sale-active) ERR-SALE-NOT-ACTIVE)
    (asserts! (<= (+ current-supply amount) (var-get max-supply)) ERR-SOLD-OUT)
    (asserts! (<= (+ (get count minter-info) amount) (var-get max-per-wallet)) ERR-MAX-MINTED)
    (map-set minted-count
      { minter: tx-sender }
      { count: (+ (get count minter-info) amount) }
    )
    (mint-batch amount)
  )
)

(define-private (mint-batch (amount uint))
  (let (
    (start-id (var-get token-counter))
  )
    (var-set token-counter (+ start-id amount))
    (fold mint-single (list u0 u1 u2 u3 u4) { count: u0, max: amount, start: start-id })
    (ok amount)
  )
)

(define-private (mint-single (index uint) (state { count: uint, max: uint, start: uint }))
  (if (< (get count state) (get max state))
    (begin
      (unwrap-panic (nft-mint? collection-nft (+ (get start state) (get count state)) tx-sender))
      { count: (+ (get count state) u1), max: (get max state), start: (get start state) }
    )
    state
  )
)

(define-public (toggle-whitelist-sale)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set whitelist-active (not (var-get whitelist-active)))
    (ok (var-get whitelist-active))
  )
)

(define-public (toggle-public-sale)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set public-sale-active (not (var-get public-sale-active)))
    (ok (var-get public-sale-active))
  )
)
