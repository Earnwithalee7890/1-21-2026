;; Task Tracker
;; Track tasks with status

(define-data-var task-count uint u0)
(define-data-var owner principal tx-sender)

(define-map tasks uint 
  {
    title: (string-ascii 100),
    done: bool,
    created: uint
  }
)

(define-read-only (get-task (id uint))
  (map-get? tasks id)
)

(define-read-only (get-task-count)
  (var-get task-count)
)

(define-public (add-task (title (string-ascii 100)))
  (let
    (
      (id (var-get task-count))
    )
    (map-set tasks id 
      {
        title: title,
        done: false,
        created: block-height
      }
    )
    (var-set task-count (+ id u1))
    (ok id)
  )
)

(define-public (complete-task (id uint))
  (let
    (
      (task (unwrap! (get-task id) (err u101)))
    )
    (map-set tasks id 
      {
        title: (get title task),
        done: true,
        created: (get created task)
      }
    )
    (ok true)
  )
)
