(define-constant CONTRACT_OWNER tx-sender)
(define-data-var total-stx-raised uint u0) ;; Total STX raised in whole units
(define-data-var softcap uint u100000) ;; Softcap of 100,000 STX
(define-data-var refunds-enabled bool false)
(define-map buyer-contributions
  { user: principal }
  { stx-amount: uint, cat-amount: uint }) ;; STX and CAT in whole units

(define-read-only (get-contributions (user principal))
  (default-to { stx-amount: u0, cat-amount: u0 }
    (map-get? buyer-contributions { user: user })))

(define-public (buy-tokens (stx-amount uint))
  (let
    ((buyer tx-sender)
     (amount-in-micro-stx (* stx-amount u1000000))
     (current-contrib (get-contributions buyer))
     (new-stx-amount (+ stx-amount (get stx-amount current-contrib)))
     (new-cat-amount (+ (* stx-amount u1000) (get cat-amount current-contrib)))) ;; Example: 1000 CAT per STX
    (asserts! (not (var-get refunds-enabled)) (err u6)) ;; Refunds must be disabled
    (asserts! (> stx-amount u0) (err u7)) ;; Amount must be positive
    (match (stx-transfer? amount-in-micro-stx buyer (as-contract tx-sender))
      success
        (begin
          (var-set total-stx-raised (+ (var-get total-stx-raised) stx-amount))
          (map-set buyer-contributions { user: buyer }
            { stx-amount: new-stx-amount, cat-amount: new-cat-amount })
          (ok true))
      error (err u8)))) ;; Transfer failed

(define-public (enable-refund)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err u9)) ;; Only owner
    (var-set refunds-enabled true)
    (ok true)))

(define-public (claim-refund)
  (let
    ((contribution (get-contributions tx-sender))
     (stx-to-refund (get stx-amount contribution))
     (amount-in-micro-stx (* stx-to-refund u1000000)))
    (asserts! (var-get refunds-enabled) (err u11)) ;; Refunds must be enabled
    (asserts! (> stx-to-refund u0) (err u12)) ;; Must have something to refund
    (match (as-contract (stx-transfer? amount-in-micro-stx tx-sender tx-sender))
      success
        (begin
          (var-set total-stx-raised (- (var-get total-stx-raised) stx-to-refund)) ;; Update total
          (map-set buyer-contributions { user: tx-sender } { stx-amount: u0, cat-amount: u0 })
          (ok true))
      error (err u13)))) ;; Transfer failed

(define-public (withdraw)
  (let
    ((amount (var-get total-stx-raised)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err u14)) ;; Only owner
    (asserts! (> amount u0) (err u15)) ;; Must have funds
    (match (as-contract (stx-transfer? (* amount u1000000) tx-sender CONTRACT_OWNER))
      success
        (begin
          (var-set total-stx-raised u0)
          (ok true))
      error (err u16)))) ;; Transfer failed

(define-read-only (get-total-stx-raised)
  (var-get total-stx-raised))

(define-read-only (are-refunds-enabled)
  (var-get refunds-enabled))