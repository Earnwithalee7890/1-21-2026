;; Tip Jar - Clarity 4
;; Simple tip collection

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u1000))
(define-constant err-unauthorized (err u1001))

(define-data-var jar-nonce uint u0)

(define-map tip-jars
    uint
    {
        owner: principal,
        name: (string-utf8 50),
        message: (string-utf8 200),
        total-tips: uint,
        tip-count: uint
    }
)

(define-read-only (get-jar (jar-id uint))
    (map-get? tip-jars jar-id)
)

(define-read-only (get-jar-count)
    (var-get jar-nonce)
)

(define-public (create-jar
    (name (string-utf8 50))
    (message (string-utf8 200))
)
    (let (
        (jar-id (var-get jar-nonce))
    )
        (map-set tip-jars jar-id {
            owner: tx-sender,
            name: name,
            message: message,
            total-tips: u0,
            tip-count: u0
        })
        (var-set jar-nonce (+ jar-id u1))
        (ok jar-id)
    )
)

(define-public (tip (jar-id uint) (amount uint))
    (let (
        (jar (unwrap! (get-jar jar-id) err-not-found))
    )
        (try! (stx-transfer? amount tx-sender (get owner jar)))
        (ok (map-set tip-jars jar-id
            (merge jar {
                total-tips: (+ (get total-tips jar) amount),
                tip-count: (+ (get tip-count jar) u1)
            })
        ))
    )
)

(define-public (update-message (jar-id uint) (new-message (string-utf8 200)))
    (let (
        (jar (unwrap! (get-jar jar-id) err-not-found))
    )
        (asserts! (is-eq tx-sender (get owner jar)) err-unauthorized)
        (ok (map-set tip-jars jar-id (merge jar {message: new-message})))
    )
)
