;; Fractional NFT Vault
;; Split NFT ownership into fungible tokens

(define-fungible-token fraction-token)

(define-map vaults
  { vault-id: uint }
  {
    nft-contract: principal,
    token-id: uint,
    curator: principal,
    total-fractions: uint,
    buyout-price: uint,
    buyout-active: bool,
    buyout-bidder: (optional principal)
  }
)

(define-map vault-holders
  { vault-id: uint, holder: principal }
  { balance: uint }
)

(define-data-var vault-nonce uint u0)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-VAULT-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-BUYOUT-ACTIVE (err u103))
(define-constant ERR-BUYOUT-NOT-ACTIVE (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))

(define-read-only (get-vault (vault-id uint))
  (map-get? vaults { vault-id: vault-id })
)

(define-read-only (get-holder-balance (vault-id uint) (holder principal))
  (default-to { balance: u0 }
    (map-get? vault-holders { vault-id: vault-id, holder: holder }))
)

(define-read-only (calculate-fraction-value (vault-id uint) (fraction-amount uint))
  (let (
    (vault (unwrap-panic (get-vault vault-id)))
  )
    (/ (* (get buyout-price vault) fraction-amount) (get total-fractions vault))
  )
)

(define-public (create-vault (nft-contract principal) (token-id uint) (total-fractions uint) (initial-buyout-price uint))
  (let (
    (vault-id (var-get vault-nonce))
  )
    (map-set vaults
      { vault-id: vault-id }
      {
        nft-contract: nft-contract,
        token-id: token-id,
        curator: tx-sender,
        total-fractions: total-fractions,
        buyout-price: initial-buyout-price,
        buyout-active: false,
        buyout-bidder: none
      }
    )
    (map-set vault-holders
      { vault-id: vault-id, holder: tx-sender }
      { balance: total-fractions }
    )
    (try! (ft-mint? fraction-token total-fractions tx-sender))
    (var-set vault-nonce (+ vault-id u1))
    (ok vault-id)
  )
)

(define-public (transfer-fractions (vault-id uint) (amount uint) (recipient principal))
  (let (
    (vault (unwrap! (get-vault vault-id) ERR-VAULT-NOT-FOUND))
    (sender-balance (get balance (get-holder-balance vault-id tx-sender)))
    (recipient-balance (get balance (get-holder-balance vault-id recipient)))
  )
    (asserts! (not (get buyout-active vault)) ERR-BUYOUT-ACTIVE)
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
    (map-set vault-holders
      { vault-id: vault-id, holder: tx-sender }
      { balance: (- sender-balance amount) }
    )
    (map-set vault-holders
      { vault-id: vault-id, holder: recipient }
      { balance: (+ recipient-balance amount) }
    )
    (ok true)
  )
)

(define-public (start-buyout (vault-id uint) (bid-price uint))
  (let (
    (vault (unwrap! (get-vault vault-id) ERR-VAULT-NOT-FOUND))
  )
    (asserts! (not (get buyout-active vault)) ERR-BUYOUT-ACTIVE)
    (asserts! (>= bid-price (get buyout-price vault)) ERR-INVALID-AMOUNT)
    (map-set vaults
      { vault-id: vault-id }
      (merge vault {
        buyout-active: true,
        buyout-price: bid-price,
        buyout-bidder: (some tx-sender)
      })
    )
    (ok true)
  )
)

(define-public (claim-buyout-proceeds (vault-id uint))
  (let (
    (vault (unwrap! (get-vault vault-id) ERR-VAULT-NOT-FOUND))
    (holder-info (get-holder-balance vault-id tx-sender))
    (proceeds (calculate-fraction-value vault-id (get balance holder-info)))
  )
    (asserts! (get buyout-active vault) ERR-BUYOUT-NOT-ACTIVE)
    (asserts! (> (get balance holder-info) u0) ERR-INSUFFICIENT-BALANCE)
    (map-set vault-holders
      { vault-id: vault-id, holder: tx-sender }
      { balance: u0 }
    )
    (ok proceeds)
  )
)

(define-public (update-buyout-price (vault-id uint) (new-price uint))
  (let (
    (vault (unwrap! (get-vault vault-id) ERR-VAULT-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get curator vault)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get buyout-active vault)) ERR-BUYOUT-ACTIVE)
    (map-set vaults
      { vault-id: vault-id }
      (merge vault { buyout-price: new-price })
    )
    (ok true)
  )
)
