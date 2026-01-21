;; Minimal Hello Contract
;; Absolute minimum contract for testing deployment

(define-data-var greeting (string-ascii 20) "Hello Stacks")

(define-read-only (say-hello)
  (var-get greeting)
)

(define-public (set-greeting (new-greeting (string-ascii 20)))
  (begin
    (var-set greeting new-greeting)
    (ok true)
  )
)
