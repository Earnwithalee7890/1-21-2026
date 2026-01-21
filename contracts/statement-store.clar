;; Statement Store
;; Store statements on chain

(define-data-var statement-count uint u0)

(define-map statements uint 
  {
    author: principal,
    text: (string-ascii 200),
    block: uint
  }
)

(define-read-only (get-statement (id uint))
  (map-get? statements id)
)

(define-read-only (get-statement-count)
  (var-get statement-count)
)

(define-public (publish (text (string-ascii 200)))
  (let
    (
      (id (var-get statement-count))
    )
    (map-set statements id 
      {
        author: tx-sender,
        text: text,
        block: block-height
      }
    )
    (var-set statement-count (+ id u1))
    (ok id)
  )
)
