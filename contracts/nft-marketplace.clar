;; NFT Marketplace
;; Buy, sell, and auction NFTs with royalty support

(define-map listings
  { nft-contract: principal, token-id: uint }
  {
    seller: principal,
    price: uint,
    royalty-percent: uint,
    royalty-recipient: principal,
    listed-at: uint
  }
)

(define-map auctions
  { auction-id: uint }
  {
    nft-contract: principal,
    token-id: uint,
    seller: principal,
    start-price: uint,
    current-bid: uint,
    highest-bidder: (optional principal),
    end-block: uint,
    royalty-percent: uint,
    royalty-recipient: principal
  }
)

(define-map bids
  { auction-id: uint, bidder: principal }
  { amount: uint }
)

(define-data-var auction-nonce uint u0)
(define-data-var platform-fee uint u250) ;; 2.5%
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-LISTED (err u101))
(define-constant ERR-ALREADY-LISTED (err u102))
(define-constant ERR-BID-TOO-LOW (err u103))
(define-constant ERR-AUCTION-ENDED (err u104))
(define-constant ERR-AUCTION-NOT-ENDED (err u105))

(define-read-only (get-listing (nft-contract principal) (token-id uint))
  (map-get? listings { nft-contract: nft-contract, token-id: token-id })
)

(define-read-only (get-auction (auction-id uint))
  (map-get? auctions { auction-id: auction-id })
)

(define-read-only (calculate-fees (price uint) (royalty-percent uint))
  (let (
    (platform-cut (/ (* price (var-get platform-fee)) u10000))
    (royalty-cut (/ (* price royalty-percent) u10000))
  )
    { platform-fee: platform-cut, royalty: royalty-cut, seller-amount: (- price (+ platform-cut royalty-cut)) }
  )
)

(define-public (list-nft (nft-contract principal) (token-id uint) (price uint) (royalty-percent uint) (royalty-recipient principal))
  (begin
    (asserts! (is-none (get-listing nft-contract token-id)) ERR-ALREADY-LISTED)
    (map-set listings
      { nft-contract: nft-contract, token-id: token-id }
      {
        seller: tx-sender,
        price: price,
        royalty-percent: royalty-percent,
        royalty-recipient: royalty-recipient,
        listed-at: block-height
      }
    )
    (ok true)
  )
)

(define-public (buy-nft (nft-contract principal) (token-id uint))
  (let (
    (listing (unwrap! (get-listing nft-contract token-id) ERR-NOT-LISTED))
    (fees (calculate-fees (get price listing) (get royalty-percent listing)))
  )
    (map-delete listings { nft-contract: nft-contract, token-id: token-id })
    (ok fees)
  )
)

(define-public (delist-nft (nft-contract principal) (token-id uint))
  (let (
    (listing (unwrap! (get-listing nft-contract token-id) ERR-NOT-LISTED))
  )
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    (map-delete listings { nft-contract: nft-contract, token-id: token-id })
    (ok true)
  )
)

(define-public (create-auction (nft-contract principal) (token-id uint) (start-price uint) (duration-blocks uint) (royalty-percent uint) (royalty-recipient principal))
  (let (
    (auction-id (var-get auction-nonce))
  )
    (map-set auctions
      { auction-id: auction-id }
      {
        nft-contract: nft-contract,
        token-id: token-id,
        seller: tx-sender,
        start-price: start-price,
        current-bid: u0,
        highest-bidder: none,
        end-block: (+ block-height duration-blocks),
        royalty-percent: royalty-percent,
        royalty-recipient: royalty-recipient
      }
    )
    (var-set auction-nonce (+ auction-id u1))
    (ok auction-id)
  )
)

(define-public (place-bid (auction-id uint) (amount uint))
  (let (
    (auction (unwrap! (get-auction auction-id) ERR-NOT-LISTED))
  )
    (asserts! (<= block-height (get end-block auction)) ERR-AUCTION-ENDED)
    (asserts! (> amount (get current-bid auction)) ERR-BID-TOO-LOW)
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        current-bid: amount,
        highest-bidder: (some tx-sender)
      })
    )
    (map-set bids
      { auction-id: auction-id, bidder: tx-sender }
      { amount: amount }
    )
    (ok true)
  )
)

(define-public (settle-auction (auction-id uint))
  (let (
    (auction (unwrap! (get-auction auction-id) ERR-NOT-LISTED))
  )
    (asserts! (> block-height (get end-block auction)) ERR-AUCTION-NOT-ENDED)
    (map-delete auctions { auction-id: auction-id })
    (ok { winner: (get highest-bidder auction), amount: (get current-bid auction) })
  )
)
