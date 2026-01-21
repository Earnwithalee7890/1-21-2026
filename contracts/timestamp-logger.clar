;; Timestamp Logger
;; Log events with block timestamps

(define-data-var event-count uint u0)

(define-map events uint 
  {
    logger: principal,
    event-type: (string-ascii 50),
    block: uint
  }
)

(define-read-only (get-event (id uint))
  (map-get? events id)
)

(define-read-only (get-event-count)
  (var-get event-count)
)

(define-read-only (get-current-block)
  block-height
)

(define-public (log-event (event-type (string-ascii 50)))
  (let
    (
      (id (var-get event-count))
    )
    (map-set events id 
      {
        logger: tx-sender,
        event-type: event-type,
        block: block-height
      }
    )
    (var-set event-count (+ id u1))
    (ok id)
  )
)
