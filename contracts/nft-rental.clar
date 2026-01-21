;; NFT Rental Protocol
;; Rent NFTs for temporary use without ownership transfer

(define-map rental-listings
  { nft-contract: principal, token-id: uint }
  {
    owner: principal,
    daily-rate: uint,
    max-duration: uint,
    min-duration: uint,
    available: bool
  }
)

(define-map active-rentals
  { rental-id: uint }
  {
    nft-contract: principal,
    token-id: uint,
    owner: principal,
    renter: principal,
    start-block: uint,
    end-block: uint,
    total-payment: uint
  }
)

(define-data-var rental-nonce uint u0)
(define-data-var platform-fee uint u500)
(define-constant BLOCKS-PER-DAY u144)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-LISTED (err u101))
(define-constant ERR-ALREADY-RENTED (err u102))
(define-constant ERR-INVALID-DURATION (err u103))
(define-constant ERR-RENTAL-ACTIVE (err u104))
(define-constant ERR-RENTAL-NOT-FOUND (err u105))

(define-read-only (get-listing (nft-contract principal) (token-id uint))
  (map-get? rental-listings { nft-contract: nft-contract, token-id: token-id })
)

(define-read-only (get-rental (rental-id uint))
  (map-get? active-rentals { rental-id: rental-id })
)

(define-read-only (calculate-rental-cost (daily-rate uint) (duration-days uint))
  (* daily-rate duration-days)
)

(define-read-only (is-rental-expired (rental-id uint))
  (let (
    (rental (unwrap-panic (get-rental rental-id)))
  )
    (> block-height (get end-block rental))
  )
)

(define-public (list-for-rent (nft-contract principal) (token-id uint) (daily-rate uint) (min-days uint) (max-days uint))
  (begin
    (map-set rental-listings
      { nft-contract: nft-contract, token-id: token-id }
      {
        owner: tx-sender,
        daily-rate: daily-rate,
        max-duration: max-days,
        min-duration: min-days,
        available: true
      }
    )
    (ok true)
  )
)

(define-public (rent-nft (nft-contract principal) (token-id uint) (duration-days uint))
  (let (
    (listing (unwrap! (get-listing nft-contract token-id) ERR-NOT-LISTED))
    (rental-id (var-get rental-nonce))
    (total-cost (calculate-rental-cost (get daily-rate listing) duration-days))
    (duration-blocks (* duration-days BLOCKS-PER-DAY))
  )
    (asserts! (get available listing) ERR-ALREADY-RENTED)
    (asserts! (and (>= duration-days (get min-duration listing)) (<= duration-days (get max-duration listing))) ERR-INVALID-DURATION)
    (map-set rental-listings
      { nft-contract: nft-contract, token-id: token-id }
      (merge listing { available: false })
    )
    (map-set active-rentals
      { rental-id: rental-id }
      {
        nft-contract: nft-contract,
        token-id: token-id,
        owner: (get owner listing),
        renter: tx-sender,
        start-block: block-height,
        end-block: (+ block-height duration-blocks),
        total-payment: total-cost
      }
    )
    (var-set rental-nonce (+ rental-id u1))
    (ok rental-id)
  )
)

(define-public (return-nft (rental-id uint))
  (let (
    (rental (unwrap! (get-rental rental-id) ERR-RENTAL-NOT-FOUND))
    (listing-key { nft-contract: (get nft-contract rental), token-id: (get token-id rental) })
    (listing (unwrap! (get-listing (get nft-contract rental) (get token-id rental)) ERR-NOT-LISTED))
  )
    (asserts! (or (is-eq tx-sender (get renter rental)) (is-rental-expired rental-id)) ERR-NOT-AUTHORIZED)
    (map-set rental-listings
      listing-key
      (merge listing { available: true })
    )
    (map-delete active-rentals { rental-id: rental-id })
    (ok true)
  )
)

(define-public (cancel-listing (nft-contract principal) (token-id uint))
  (let (
    (listing (unwrap! (get-listing nft-contract token-id) ERR-NOT-LISTED))
  )
    (asserts! (is-eq tx-sender (get owner listing)) ERR-NOT-AUTHORIZED)
    (asserts! (get available listing) ERR-RENTAL-ACTIVE)
    (map-delete rental-listings { nft-contract: nft-contract, token-id: token-id })
    (ok true)
  )
)
