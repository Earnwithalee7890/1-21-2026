;; Multi-Sig Treasury
;; Manage funds with multi-signature approval

(define-map signers
  { signer: principal }
  { active: bool, weight: uint }
)

(define-map transactions
  { tx-id: uint }
  {
    proposer: principal,
    recipient: principal,
    amount: uint,
    memo: (string-ascii 100),
    approvals: uint,
    executed: bool,
    created-at: uint
  }
)

(define-map transaction-approvals
  { tx-id: uint, signer: principal }
  { approved: bool }
)

(define-data-var tx-nonce uint u0)
(define-data-var required-approvals uint u3)
(define-data-var total-signers uint u0)
(define-data-var treasury-balance uint u0)
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-SIGNER (err u101))
(define-constant ERR-TX-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-APPROVED (err u103))
(define-constant ERR-ALREADY-EXECUTED (err u104))
(define-constant ERR-INSUFFICIENT-APPROVALS (err u105))
(define-constant ERR-INSUFFICIENT-FUNDS (err u106))

(define-read-only (get-signer (signer principal))
  (map-get? signers { signer: signer })
)

(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions { tx-id: tx-id })
)

(define-read-only (has-approved (tx-id uint) (signer principal))
  (default-to { approved: false }
    (map-get? transaction-approvals { tx-id: tx-id, signer: signer }))
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (is-signer (address principal))
  (match (get-signer address)
    signer-data (get active signer-data)
    false
  )
)

(define-public (add-signer (new-signer principal) (weight uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set signers
      { signer: new-signer }
      { active: true, weight: weight }
    )
    (var-set total-signers (+ (var-get total-signers) u1))
    (ok true)
  )
)

(define-public (remove-signer (signer-to-remove principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set signers
      { signer: signer-to-remove }
      { active: false, weight: u0 }
    )
    (var-set total-signers (- (var-get total-signers) u1))
    (ok true)
  )
)

(define-public (deposit (amount uint))
  (begin
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok (var-get treasury-balance))
  )
)

(define-public (propose-transaction (recipient principal) (amount uint) (memo (string-ascii 100)))
  (let (
    (tx-id (var-get tx-nonce))
  )
    (asserts! (is-signer tx-sender) ERR-NOT-SIGNER)
    (asserts! (<= amount (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
    (map-set transactions
      { tx-id: tx-id }
      {
        proposer: tx-sender,
        recipient: recipient,
        amount: amount,
        memo: memo,
        approvals: u1,
        executed: false,
        created-at: block-height
      }
    )
    (map-set transaction-approvals
      { tx-id: tx-id, signer: tx-sender }
      { approved: true }
    )
    (var-set tx-nonce (+ tx-id u1))
    (ok tx-id)
  )
)

(define-public (approve-transaction (tx-id uint))
  (let (
    (tx (unwrap! (get-transaction tx-id) ERR-TX-NOT-FOUND))
    (approval-status (has-approved tx-id tx-sender))
  )
    (asserts! (is-signer tx-sender) ERR-NOT-SIGNER)
    (asserts! (not (get approved approval-status)) ERR-ALREADY-APPROVED)
    (asserts! (not (get executed tx)) ERR-ALREADY-EXECUTED)
    (map-set transaction-approvals
      { tx-id: tx-id, signer: tx-sender }
      { approved: true }
    )
    (map-set transactions
      { tx-id: tx-id }
      (merge tx { approvals: (+ (get approvals tx) u1) })
    )
    (ok (+ (get approvals tx) u1))
  )
)

(define-public (execute-transaction (tx-id uint))
  (let (
    (tx (unwrap! (get-transaction tx-id) ERR-TX-NOT-FOUND))
  )
    (asserts! (not (get executed tx)) ERR-ALREADY-EXECUTED)
    (asserts! (>= (get approvals tx) (var-get required-approvals)) ERR-INSUFFICIENT-APPROVALS)
    (asserts! (<= (get amount tx) (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
    (var-set treasury-balance (- (var-get treasury-balance) (get amount tx)))
    (map-set transactions
      { tx-id: tx-id }
      (merge tx { executed: true })
    )
    (ok true)
  )
)

(define-public (set-required-approvals (new-required uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set required-approvals new-required)
    (ok new-required)
  )
)
