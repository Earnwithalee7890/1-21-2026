;; Message Board
;; Post and read messages

(define-data-var message-count uint u0)

(define-map messages uint 
  {
    author: principal,
    content: (string-ascii 200),
    block: uint
  }
)

(define-read-only (get-message-count)
  (var-get message-count)
)

(define-read-only (get-message (id uint))
  (map-get? messages id)
)

(define-public (post-message (content (string-ascii 200)))
  (let
    (
      (id (var-get message-count))
    )
    (map-set messages id 
      {
        author: tx-sender,
        content: content,
        block: block-height
      }
    )
    (var-set message-count (+ id u1))
    (ok id)
  )
)
