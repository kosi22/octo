;; StorageAuditor Smart Contract
;; Purpose: Audit storage node claims independently with third-party verification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-invalid-node (err u102))
(define-constant err-already-audited (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-audit-not-found (err u105))
(define-constant err-invalid-audit-status (err u106))
(define-constant err-already-rewarded (err u107))

;; Minimum stake required to become an auditor (in microSTX)
(define-constant min-auditor-stake u1000000) ;; 1 STX

;; Reward amount for successful audits (in microSTX)
(define-constant audit-reward u100000) ;; 0.1 STX

;; Data Variables
(define-data-var next-audit-id uint u1)
(define-data-var total-auditors uint u0)

;; Data Maps

;; Registered auditors
(define-map auditors
    { auditor: principal }
    {
        stake: uint,
        reputation-score: uint,
        total-audits: uint,
        successful-audits: uint,
        is-active: bool,
        registered-at: uint
    }
)

;; Storage nodes to be audited
(define-map storage-nodes
    { node-id: (string-ascii 64) }
    {
        owner: principal,
        claimed-data-hash: (buff 32),
        claimed-size: uint,
        last-audit-block: uint,
        is-active: bool
    }
)

;; Audit reports
(define-map audit-reports
    { audit-id: uint }
    {
        auditor: principal,
        node-id: (string-ascii 64),
        data-available: bool,
        data-hash-verified: bool,
        size-verified: bool,
        audit-timestamp: uint,
        proof-hash: (buff 32),
        status: (string-ascii 20), ;; "pending", "verified", "disputed", "rewarded"
        block-height: uint
    }
)

;; Audit assignments (to prevent duplicate audits)
(define-map audit-assignments
    { node-id: (string-ascii 64), auditor: principal }
    { audit-id: uint, assigned-at: uint }
)

;; Public Functions

;; Register as an auditor
(define-public (register-auditor (stake uint))
    (let (
        (auditor tx-sender)
        (current-block block-height)
    )
        (asserts! (not (var-get registration-paused)) err-unauthorized)
        (asserts! (>= stake min-auditor-stake) err-insufficient-stake)
        (asserts! (<= stake u100000000000) err-insufficient-stake) ;; Max stake limit
        (asserts! (is-none (map-get? auditors {auditor: auditor})) err-unauthorized)
        
        ;; Transfer stake to contract
        (try! (stx-transfer? stake auditor (as-contract tx-sender)))
        
        ;; Register auditor
        (map-set auditors {auditor: auditor} {
            stake: stake,
            reputation-score: u100, ;; Start with base score
            total-audits: u0,
            successful-audits: u0,
            is-active: true,
            registered-at: current-block
        })
        
        (var-set total-auditors (+ (var-get total-auditors) u1))
        (ok true)
    )
)

;; Register a storage node for auditing
(define-public (register-storage-node (node-id (string-ascii 64)) (data-hash (buff 32)) (size uint))
    (let (
        (node-owner tx-sender)
        (current-block block-height)
    )
        (asserts! (> (len node-id) u0) err-invalid-node) ;; Validate node-id is not empty
        (asserts! (<= size u1000000000000) err-invalid-node) ;; Max size limit (1TB in bytes)
        (asserts! (> size u0) err-invalid-node) ;; Size must be positive
        (asserts! (is-none (map-get? storage-nodes {node-id: node-id})) err-invalid-node)
        
        (map-set storage-nodes {node-id: node-id} {
            owner: node-owner,
            claimed-data-hash: data-hash,
            claimed-size: size,
            last-audit-block: current-block,
            is-active: true
        })
        
        (ok true)
    )
)

;; Submit an audit report
(define-public (submit-audit-report 
    (node-id (string-ascii 64)) 
    (data-available bool) 
    (data-hash-verified bool) 
    (size-verified bool) 
    (proof-hash (buff 32))
)
    (let (
        (auditor tx-sender)
        (audit-id (var-get next-audit-id))
        (current-block block-height)
        (node-data (unwrap! (map-get? storage-nodes {node-id: node-id}) err-invalid-node))
        (auditor-data (unwrap! (map-get? auditors {auditor: auditor}) err-unauthorized))
    )
        ;; Input validation
        (asserts! (> (len node-id) u0) err-invalid-node) ;; Validate node-id is not empty
        (asserts! (not (is-eq proof-hash 0x)) err-invalid-audit-status) ;; Proof hash cannot be empty
        
        ;; Verify auditor is registered and active
        (asserts! (get is-active auditor-data) err-unauthorized)
        (asserts! (get is-active node-data) err-invalid-node) ;; Node must be active
        
        ;; Check if this auditor has already audited this node recently
        (asserts! (is-none (map-get? audit-assignments {node-id: node-id, auditor: auditor})) err-already-audited)
        
        ;; Create audit report
        (map-set audit-reports {audit-id: audit-id} {
            auditor: auditor,
            node-id: node-id,
            data-available: data-available,
            data-hash-verified: data-hash-verified,
            size-verified: size-verified,
            audit-timestamp: current-block,
            proof-hash: proof-hash,
            status: "pending",
            block-height: current-block
        })
        
        ;; Record audit assignment
        (map-set audit-assignments {node-id: node-id, auditor: auditor} {
            audit-id: audit-id,
            assigned-at: current-block
        })
        
        ;; Update auditor stats
        (map-set auditors {auditor: auditor} 
            (merge auditor-data {
                total-audits: (+ (get total-audits auditor-data) u1)
            })
        )
        
        ;; Increment audit ID
        (var-set next-audit-id (+ audit-id u1))
        
        (ok audit-id)
    )
)

;; Verify and reward audit report (called by contract owner or automated system)
(define-public (verify-and-reward-audit (audit-id uint) (is-honest bool))
    (let (
        (audit-data (unwrap! (map-get? audit-reports {audit-id: audit-id}) err-audit-not-found))
        (auditor (get auditor audit-data))
        (auditor-data (unwrap! (map-get? auditors {auditor: auditor}) err-unauthorized))
        (current-status (get status audit-data))
    )
        ;; Input validation
        (asserts! (> audit-id u0) err-audit-not-found) ;; Audit ID must be positive
        (asserts! (< audit-id (var-get next-audit-id)) err-audit-not-found) ;; Audit ID must exist
        
        (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (as-contract tx-sender))) err-owner-only)
        (asserts! (is-eq current-status "pending") err-invalid-audit-status)
        
        (if is-honest
            (begin
                ;; Reward honest auditor
                (try! (as-contract (stx-transfer? audit-reward tx-sender auditor)))
                
                ;; Update audit status
                (map-set audit-reports {audit-id: audit-id}
                    (merge audit-data {status: "rewarded"})
                )
                
                ;; Update auditor reputation
                (map-set auditors {auditor: auditor}
                    (merge auditor-data {
                        successful-audits: (+ (get successful-audits auditor-data) u1),
                        reputation-score: (+ (get reputation-score auditor-data) u10)
                    })
                )
                
                (ok true)
            )
            (begin
                ;; Mark as disputed (could implement slashing here)
                (map-set audit-reports {audit-id: audit-id}
                    (merge audit-data {status: "disputed"})
                )
                
                ;; Reduce auditor reputation
                (map-set auditors {auditor: auditor}
                    (merge auditor-data {
                        reputation-score: (if (> (get reputation-score auditor-data) u10) 
                            (- (get reputation-score auditor-data) u10) 
                            u0)
                    })
                )
                
                (ok false)
            )
        )
    )
)

;; Withdraw auditor stake (deactivate auditor)
(define-public (withdraw-stake)
    (let (
        (auditor tx-sender)
        (auditor-data (unwrap! (map-get? auditors {auditor: auditor}) err-unauthorized))
        (stake-amount (get stake auditor-data))
    )
        (asserts! (get is-active auditor-data) err-unauthorized)
        
        ;; Deactivate auditor
        (map-set auditors {auditor: auditor}
            (merge auditor-data {is-active: false})
        )
        
        ;; Return stake
        (try! (as-contract (stx-transfer? stake-amount tx-sender auditor)))
        
        (var-set total-auditors (- (var-get total-auditors) u1))
        (ok stake-amount)
    )
)

;; Read-only functions

;; Get auditor information
(define-read-only (get-auditor-info (auditor principal))
    (map-get? auditors {auditor: auditor})
)

;; Get storage node information
(define-read-only (get-storage-node-info (node-id (string-ascii 64)))
    (map-get? storage-nodes {node-id: node-id})
)

;; Get audit report
(define-read-only (get-audit-report (audit-id uint))
    (map-get? audit-reports {audit-id: audit-id})
)

;; Get audit assignment
(define-read-only (get-audit-assignment (node-id (string-ascii 64)) (auditor principal))
    (map-get? audit-assignments {node-id: node-id, auditor: auditor})
)

;; Get total number of auditors
(define-read-only (get-total-auditors)
    (var-get total-auditors)
)

;; Get next audit ID
(define-read-only (get-next-audit-id)
    (var-get next-audit-id)
)

;; Check if auditor can audit a specific node
(define-read-only (can-audit-node (auditor principal) (node-id (string-ascii 64)))
    (let (
        (auditor-data (map-get? auditors {auditor: auditor}))
        (existing-assignment (map-get? audit-assignments {node-id: node-id, auditor: auditor}))
    )
        (and 
            (is-some auditor-data)
            (get is-active (unwrap-panic auditor-data))
            (is-none existing-assignment)
        )
    )
)

;; Admin functions

;; Update minimum stake requirement
(define-public (update-min-stake (new-stake uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        ;; This would require updating the constant, which isn't possible in Clarity
        ;; In practice, you'd deploy a new contract version
        (ok true)
    )
)

;; Emergency pause auditor registration
(define-data-var registration-paused bool false)

(define-public (pause-registration (paused bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set registration-paused paused)
        (ok true)
    )
)

(define-read-only (is-registration-paused)
    (var-get registration-paused)
)