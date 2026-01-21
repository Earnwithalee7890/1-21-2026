;; Dynamic NFT Collection
;; NFTs with evolving metadata based on on-chain activity

(define-non-fungible-token dynamic-nft uint)

(define-map nft-metadata
  { token-id: uint }
  {
    owner: principal,
    level: uint,
    experience: uint,
    traits: (list 5 (string-ascii 20)),
    last-evolution: uint,
    created-at: uint
  }
)

(define-map evolution-thresholds
  { level: uint }
  { xp-required: uint, trait-bonus: (string-ascii 20) }
)

(define-data-var token-counter uint u0)
(define-data-var base-uri (string-ascii 100) "https://api.dynamic-nft.com/metadata/")
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-XP (err u102))
(define-constant ERR-MAX-LEVEL (err u103))
(define-constant MAX-LEVEL u10)

(define-read-only (get-nft-data (token-id uint))
  (map-get? nft-metadata { token-id: token-id })
)

(define-read-only (get-level-requirement (level uint))
  (map-get? evolution-thresholds { level: level })
)

(define-read-only (get-owner (token-id uint))
  (nft-get-owner? dynamic-nft token-id)
)

(define-read-only (get-token-uri (token-id uint))
  (ok (some (var-get base-uri)))
)

(define-public (mint (recipient principal) (initial-traits (list 5 (string-ascii 20))))
  (let (
    (token-id (var-get token-counter))
  )
    (try! (nft-mint? dynamic-nft token-id recipient))
    (map-set nft-metadata
      { token-id: token-id }
      {
        owner: recipient,
        level: u1,
        experience: u0,
        traits: initial-traits,
        last-evolution: block-height,
        created-at: block-height
      }
    )
    (var-set token-counter (+ token-id u1))
    (ok token-id)
  )
)

(define-public (add-experience (token-id uint) (xp-amount uint))
  (let (
    (nft-data (unwrap! (get-nft-data token-id) ERR-NOT-FOUND))
    (owner (unwrap! (get-owner token-id) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    (map-set nft-metadata
      { token-id: token-id }
      (merge nft-data {
        experience: (+ (get experience nft-data) xp-amount)
      })
    )
    (ok (+ (get experience nft-data) xp-amount))
  )
)

(define-public (evolve (token-id uint))
  (let (
    (nft-data (unwrap! (get-nft-data token-id) ERR-NOT-FOUND))
    (owner (unwrap! (get-owner token-id) ERR-NOT-FOUND))
    (next-level (+ (get level nft-data) u1))
    (threshold (unwrap! (get-level-requirement next-level) ERR-MAX-LEVEL))
  )
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    (asserts! (< (get level nft-data) MAX-LEVEL) ERR-MAX-LEVEL)
    (asserts! (>= (get experience nft-data) (get xp-required threshold)) ERR-INSUFFICIENT-XP)
    (map-set nft-metadata
      { token-id: token-id }
      (merge nft-data {
        level: next-level,
        experience: (- (get experience nft-data) (get xp-required threshold)),
        last-evolution: block-height
      })
    )
    (ok next-level)
  )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (let (
    (nft-data (unwrap! (get-nft-data token-id) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (try! (nft-transfer? dynamic-nft token-id sender recipient))
    (map-set nft-metadata
      { token-id: token-id }
      (merge nft-data { owner: recipient })
    )
    (ok true)
  )
)

(define-public (set-evolution-threshold (level uint) (xp-required uint) (trait-bonus (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set evolution-thresholds
      { level: level }
      { xp-required: xp-required, trait-bonus: trait-bonus }
    )
    (ok true)
  )
)
