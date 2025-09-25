;; Multi-Send Atomic Transfer Contract
;; Enables batch transfers with all-or-nothing atomicity

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-TRANSFER-FAILED (err u103))
(define-constant ERR-EMPTY-RECIPIENTS (err u104))
(define-constant ERR-MAX-RECIPIENTS-EXCEEDED (err u105))

;; Constants
(define-constant MAX-RECIPIENTS u50) ;; Maximum number of recipients per batch

;; Data structures
(define-map transfer-logs
  { batch-id: uint }
  {
    sender: principal,
    total-amount: uint,
    recipient-count: uint,
    timestamp: uint,
    success: bool
  }
)

;; Private variables
(define-data-var batch-counter uint u0)

;; Transfer recipient structure is defined inline as {recipient: principal, amount: uint}

;; Helper function to validate transfer amount
(define-private (is-valid-amount (amount uint))
  (> amount u0)
)

;; Helper function to validate recipient list
(define-private (validate-recipients (recipients (list 50 {recipient: principal, amount: uint})))
  (let ((recipient-count (len recipients)))
    (and 
      (> recipient-count u0)
      (<= recipient-count MAX-RECIPIENTS)
    )
  )
)

;; Helper function to calculate total amount needed
(define-private (sum-amounts (recipients (list 50 {recipient: principal, amount: uint})))
  (fold + (map get-amount recipients) u0)
)

;; Helper function to extract amount from recipient record
(define-private (get-amount (recipient {recipient: principal, amount: uint}))
  (get amount recipient)
)

;; Helper function to perform a single STX transfer
(define-private (execute-stx-transfer (transfer-data {recipient: principal, amount: uint}) (sender principal))
  (let ((recipient (get recipient transfer-data))
        (amount (get amount transfer-data)))
    (if (is-valid-amount amount)
      (stx-transfer? amount sender recipient)
      ERR-INVALID-AMOUNT
    )
  )
)

;; Helper function to validate all amounts in the list
(define-private (validate-all-amounts (recipients (list 50 {recipient: principal, amount: uint})))
  (fold and (map validate-single-amount recipients) true)
)

;; Helper function to validate a single amount
(define-private (validate-single-amount (recipient {recipient: principal, amount: uint}))
  (is-valid-amount (get amount recipient))
)

;; Main function: Multi-send STX with atomicity
(define-public (multi-send-stx (recipients (list 50 {recipient: principal, amount: uint})))
  (let (
    (sender tx-sender)
    (total-amount (sum-amounts recipients))
    (recipient-count (len recipients))
    (current-batch (+ (var-get batch-counter) u1))
    (sender-balance (stx-get-balance sender))
  )
    ;; Validate inputs
    (asserts! (validate-recipients recipients) ERR-EMPTY-RECIPIENTS)
    (asserts! (validate-all-amounts recipients) ERR-INVALID-AMOUNT)
    (asserts! (>= sender-balance total-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Execute all transfers atomically
    (match (fold execute-transfer-fold recipients (ok true))
      success-flag 
        (begin
          ;; Update batch counter
          (var-set batch-counter current-batch)
          ;; Log successful batch
          (map-set transfer-logs 
            { batch-id: current-batch }
            {
              sender: sender,
              total-amount: total-amount,
              recipient-count: recipient-count,
              timestamp: block-height,
              success: true
            }
          )
          (ok { 
            batch-id: current-batch,
            recipients-processed: recipient-count,
            total-amount: total-amount
          })
        )
      error-code (err error-code) ;; If any transfer fails, the entire transaction reverts
    )
  )
)

;; Fold function for executing transfers with early exit on failure
(define-private (execute-transfer-fold 
  (recipient {recipient: principal, amount: uint}) 
  (previous-result (response bool uint)))
  (match previous-result
    success-flag 
      (match (execute-stx-transfer recipient tx-sender)
        transfer-success (ok true) ;; Continue with next transfer
        transfer-error (err transfer-error)   ;; Propagate error to stop processing
      )
    previous-error (err previous-error) ;; Propagate previous error
  )
)

;; Read-only function to get batch information
(define-read-only (get-batch-info (batch-id uint))
  (map-get? transfer-logs { batch-id: batch-id })
)

;; Read-only function to get current batch counter
(define-read-only (get-current-batch-id)
  (var-get batch-counter)
)

;; Read-only function to validate a batch before execution (dry run)
(define-read-only (validate-batch (recipients (list 50 {recipient: principal, amount: uint})))
  (let (
    (total-amount (sum-amounts recipients))
    (recipient-count (len recipients))
  )
    {
      valid-recipients: (validate-recipients recipients),
      valid-amounts: (validate-all-amounts recipients),
      total-amount: total-amount,
      recipient-count: recipient-count,
      max-recipients: MAX-RECIPIENTS
    }
  )
)

;; Emergency function to get contract stats (read-only)
(define-read-only (get-contract-stats)
  {
    total-batches: (var-get batch-counter),
    max-recipients-per-batch: MAX-RECIPIENTS
  }
)