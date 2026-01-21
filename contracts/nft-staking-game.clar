;; Gaming NFT Staking
;; Stake gaming NFTs to earn in-game rewards

(define-map staked-nfts
  { nft-contract: principal, token-id: uint }
  {
    owner: principal,
    staked-at: uint,
    last-claim: uint,
    multiplier: uint,
    rarity: (string-ascii 10)
  }
)

(define-map user-staking-stats
  { user: principal }
  {
    total-staked: uint,
    total-earned: uint,
    total-claimed: uint
  }
)

(define-map rarity-rewards
  { rarity: (string-ascii 10) }
  { base-reward: uint, bonus-multiplier: uint }
)

(define-data-var base-reward-rate uint u100)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NFT-NOT-STAKED (err u101))
(define-constant ERR-ALREADY-STAKED (err u102))
(define-constant ERR-NOTHING-TO-CLAIM (err u103))

(define-read-only (get-staked-nft (nft-contract principal) (token-id uint))
  (map-get? staked-nfts { nft-contract: nft-contract, token-id: token-id })
)

(define-read-only (get-user-stats (user principal))
  (default-to { total-staked: u0, total-earned: u0, total-claimed: u0 }
    (map-get? user-staking-stats { user: user }))
)

(define-read-only (get-rarity-config (rarity (string-ascii 10)))
  (default-to { base-reward: u100, bonus-multiplier: u100 }
    (map-get? rarity-rewards { rarity: rarity }))
)

(define-read-only (calculate-pending-rewards (nft-contract principal) (token-id uint))
  (match (get-staked-nft nft-contract token-id)
    stake-info
      (let (
        (blocks-staked (- block-height (get last-claim stake-info)))
        (rarity-config (get-rarity-config (get rarity stake-info)))
        (base-reward (* blocks-staked (get base-reward rarity-config)))
        (with-multiplier (/ (* base-reward (get multiplier stake-info)) u100))
      )
        with-multiplier
      )
    u0
  )
)

(define-public (stake-nft (nft-contract principal) (token-id uint) (rarity (string-ascii 10)))
  (let (
    (user-stats (get-user-stats tx-sender))
  )
    (asserts! (is-none (get-staked-nft nft-contract token-id)) ERR-ALREADY-STAKED)
    (map-set staked-nfts
      { nft-contract: nft-contract, token-id: token-id }
      {
        owner: tx-sender,
        staked-at: block-height,
        last-claim: block-height,
        multiplier: u100,
        rarity: rarity
      }
    )
    (map-set user-staking-stats
      { user: tx-sender }
      (merge user-stats { total-staked: (+ (get total-staked user-stats) u1) })
    )
    (ok true)
  )
)

(define-public (unstake-nft (nft-contract principal) (token-id uint))
  (let (
    (stake-info (unwrap! (get-staked-nft nft-contract token-id) ERR-NFT-NOT-STAKED))
    (pending (calculate-pending-rewards nft-contract token-id))
    (user-stats (get-user-stats tx-sender))
  )
    (asserts! (is-eq tx-sender (get owner stake-info)) ERR-NOT-AUTHORIZED)
    (map-delete staked-nfts { nft-contract: nft-contract, token-id: token-id })
    (map-set user-staking-stats
      { user: tx-sender }
      (merge user-stats {
        total-staked: (- (get total-staked user-stats) u1),
        total-earned: (+ (get total-earned user-stats) pending),
        total-claimed: (+ (get total-claimed user-stats) pending)
      })
    )
    (ok pending)
  )
)

(define-public (claim-rewards (nft-contract principal) (token-id uint))
  (let (
    (stake-info (unwrap! (get-staked-nft nft-contract token-id) ERR-NFT-NOT-STAKED))
    (pending (calculate-pending-rewards nft-contract token-id))
    (user-stats (get-user-stats tx-sender))
  )
    (asserts! (is-eq tx-sender (get owner stake-info)) ERR-NOT-AUTHORIZED)
    (asserts! (> pending u0) ERR-NOTHING-TO-CLAIM)
    (map-set staked-nfts
      { nft-contract: nft-contract, token-id: token-id }
      (merge stake-info { last-claim: block-height })
    )
    (map-set user-staking-stats
      { user: tx-sender }
      (merge user-stats {
        total-earned: (+ (get total-earned user-stats) pending),
        total-claimed: (+ (get total-claimed user-stats) pending)
      })
    )
    (ok pending)
  )
)

(define-public (set-rarity-rewards (rarity (string-ascii 10)) (base-reward uint) (bonus uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set rarity-rewards
      { rarity: rarity }
      { base-reward: base-reward, bonus-multiplier: bonus }
    )
    (ok true)
  )
)

(define-public (boost-multiplier (nft-contract principal) (token-id uint) (new-multiplier uint))
  (let (
    (stake-info (unwrap! (get-staked-nft nft-contract token-id) ERR-NFT-NOT-STAKED))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set staked-nfts
      { nft-contract: nft-contract, token-id: token-id }
      (merge stake-info { multiplier: new-multiplier })
    )
    (ok new-multiplier)
  )
)
