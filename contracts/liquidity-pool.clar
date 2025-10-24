;; -----------------------------------------------------------
;; liquidity-pool.clar
;; -----------------------------------------------------------
;; Automated Market Maker (AMM) Liquidity Pool Management
;; Works with SIP-010 fungible tokens on Stacks (STX)
;; -----------------------------------------------------------

;; Define the SIP-010 fungible token trait
(define-trait ft-trait
  (
    ;; Transfer from the caller to a new principal
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    ;; Get the token balance of who
    (get-balance (principal) (response uint uint))
    ;; Get the total number of tokens
    (get-total-supply () (response uint uint))
    ;; Get the token decimals
    (get-decimals () (response uint uint))
    ;; Get the token name
    (get-name () (response (string-ascii 32) uint))
    ;; Get the token symbol
    (get-symbol () (response (string-ascii 32) uint))
    ;; Get the token URI
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

(define-constant ERR_UNAUTHORIZED u100)
(define-constant ERR_ZERO_AMOUNT u101)
(define-constant ERR_TRANSFER_FAIL u102)
(define-constant ERR_INSUFFICIENT_LIQUIDITY u103)
(define-constant ERR_INVALID_RATIO u104)

(define-constant FEE_BPS u30) ;; 0.3% swap fee
(define-constant BPS_DENOMINATOR u10000)

(define-data-var admin principal tx-sender)

;; Reserve balances
(define-data-var reserve-x uint u0)
(define-data-var reserve-y uint u0)

;; Liquidity shares per provider
(define-map liquidity-shares principal uint)
(define-data-var total-shares uint u0)

;; -----------------------------------------------------------
;; UTILITIES
;; -----------------------------------------------------------

(define-private (is-admin (caller principal))
  (is-eq caller (var-get admin)))

(define-private (calc-fee (amount uint))
  (/ (* amount FEE_BPS) BPS_DENOMINATOR))

(define-private (sqrt (x uint))
  (if (is-eq x u0)
    u0
    (let ((z (/ (+ x u1) u2)))
      (let ((y (/ (+ z (/ x z)) u2)))
        (let ((result (/ (+ y (/ x y)) u2)))
          result
        )
      )
    )
  )
)

(define-private (min (a uint) (b uint))
  (if (<= a b) a b))

;; -----------------------------------------------------------
;; ADD LIQUIDITY
;; -----------------------------------------------------------

(define-public (add-liquidity (token-x <ft-trait>) (token-y <ft-trait>)
                              (amount-x uint) (amount-y uint))
  (let ((total-liq (var-get total-shares))
        (current-x (var-get reserve-x))
        (current-y (var-get reserve-y)))
    (begin
      (asserts! (> amount-x u0) (err ERR_ZERO_AMOUNT))
      (asserts! (> amount-y u0) (err ERR_ZERO_AMOUNT))

      (asserts!
        (is-ok (contract-call? token-x transfer amount-x tx-sender (as-contract tx-sender) none))
        (err ERR_TRANSFER_FAIL))

      (asserts!
        (is-ok (contract-call? token-y transfer amount-y tx-sender (as-contract tx-sender) none))
        (err ERR_TRANSFER_FAIL))

      (let ((shares
              (if (is-eq total-liq u0)
                  (sqrt (* amount-x amount-y))
                  (min (/ (* amount-x total-liq) current-x)
                       (/ (* amount-y total-liq) current-y))
              )))
        (asserts! (> shares u0) (err ERR_INVALID_RATIO))

        ;; Update pool reserves
        (var-set reserve-x (+ current-x amount-x))
        (var-set reserve-y (+ current-y amount-y))
        (var-set total-shares (+ total-liq shares))

        (map-set liquidity-shares tx-sender
          (+ (default-to u0 (map-get? liquidity-shares tx-sender)) shares))

        (ok {
          provider: tx-sender,
          shares: shares,
          total-shares: (var-get total-shares)
        })
      )
    )
  )
)

;; -----------------------------------------------------------
;; REMOVE LIQUIDITY
;; -----------------------------------------------------------

(define-public (remove-liquidity (token-x <ft-trait>) (token-y <ft-trait>)
                                 (shares uint))
  (let ((user-shares (default-to u0 (map-get? liquidity-shares tx-sender)))
        (total-liq (var-get total-shares))
        (current-x (var-get reserve-x))
        (current-y (var-get reserve-y)))
    (begin
      (asserts! (> shares u0) (err ERR_ZERO_AMOUNT))
      (asserts! (<= shares user-shares) (err ERR_INSUFFICIENT_LIQUIDITY))

      (let ((amount-x (/ (* shares current-x) total-liq))
            (amount-y (/ (* shares current-y) total-liq)))
        (asserts! (> amount-x u0) (err ERR_INVALID_RATIO))
        (asserts! (> amount-y u0) (err ERR_INVALID_RATIO))

        ;; Update state
        (map-set liquidity-shares tx-sender (- user-shares shares))
        (var-set total-shares (- total-liq shares))
        (var-set reserve-x (- current-x amount-x))
        (var-set reserve-y (- current-y amount-y))

        ;; Send tokens back
        (asserts!
          (is-ok (contract-call? token-x transfer amount-x (as-contract tx-sender) tx-sender none))
          (err ERR_TRANSFER_FAIL))
        (asserts!
          (is-ok (contract-call? token-y transfer amount-y (as-contract tx-sender) tx-sender none))
          (err ERR_TRANSFER_FAIL))

        (ok {withdrawn-x: amount-x, withdrawn-y: amount-y})
      )
    )
  )
)

;; -----------------------------------------------------------
;; SWAP FUNCTION
;; -----------------------------------------------------------

(define-public (swap (token-in <ft-trait>) (token-out <ft-trait>)
                     (amount-in uint) (to principal))
  (begin
    ;; Check that tokens are different
    (asserts! (not (is-eq token-in token-out)) (err ERR_INVALID_RATIO))
    
    (let ((current-x (var-get reserve-x))
          (current-y (var-get reserve-y)))
      
      (asserts! (> amount-in u0) (err ERR_ZERO_AMOUNT))

      ;; Apply swap fee
      (let ((fee (calc-fee amount-in))
            (amount-in-net (- amount-in fee)))
        (let ((output-amount
                (/ (* amount-in-net current-y)
                   (+ current-x amount-in-net))))
          ;; Transfer input token
          (asserts!
            (is-ok (contract-call? token-in transfer amount-in tx-sender (as-contract tx-sender) none))
            (err ERR_TRANSFER_FAIL))

          ;; Transfer output token
          (asserts!
            (is-ok (contract-call? token-out transfer output-amount (as-contract tx-sender) to none))
            (err ERR_TRANSFER_FAIL))

          ;; Update reserves
          (var-set reserve-x (+ current-x amount-in-net))
          (var-set reserve-y (- current-y output-amount))

          (ok {
            trader: tx-sender,
            amount-in: amount-in,
            amount-out: output-amount,
            fee: fee
          }))))))

;; -----------------------------------------------------------
;; READ-ONLY FUNCTIONS
;; -----------------------------------------------------------

(define-read-only (get-reserves)
  {
    reserve-x: (var-get reserve-x),
    reserve-y: (var-get reserve-y)
  }
)

(define-read-only (get-liquidity (who principal))
  (ok (default-to u0 (map-get? liquidity-shares who)))
)

(define-read-only (get-total-shares)
  (ok (var-get total-shares))
)
