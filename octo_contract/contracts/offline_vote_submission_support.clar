;; Enhanced Offline Voting Smart Contract
;; Comprehensive voting system with advanced features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-VOTING-CLOSED (err u102))
(define-constant ERR-INVALID-SIGNATURE (err u103))
(define-constant ERR-VOTING-NOT-STARTED (err u104))
(define-constant ERR-INVALID-OPTION (err u105))
(define-constant ERR-REPLAY-ATTACK (err u106))
(define-constant ERR-NOT-ELIGIBLE (err u107))
(define-constant ERR-DELEGATION-NOT-ALLOWED (err u108))
(define-constant ERR-ALREADY-DELEGATED (err u109))
(define-constant ERR-INVALID-QUORUM (err u110))
(define-constant ERR-VOTING-NOT-FINALIZED (err u111))
(define-constant ERR-ALREADY-FINALIZED (err u112))
(define-constant ERR-INSUFFICIENT-STAKE (err u113))
(define-constant ERR-INVALID-THRESHOLD (err u114))
(define-constant ERR-EMERGENCY-STOP (err u115))
(define-constant ERR-INVALID-VOTE-TYPE (err u116))
(define-constant ERR-WEIGHTED-VOTING-DISABLED (err u117))

;; Contract owner and admins
(define-constant CONTRACT-OWNER tx-sender)
(define-data-var emergency-stop bool false)

;; Voting configuration
(define-data-var voting-active bool false)
(define-data-var voting-title (string-utf8 100) u"")
(define-data-var voting-description (string-utf8 500) u"")
(define-data-var voting-end-height uint u0)
(define-data-var voting-start-height uint u0)
(define-data-var voting-finalized bool false)
(define-data-var winning-option uint u0)

;; Advanced voting features
(define-data-var require-registration bool false)
(define-data-var allow-vote-delegation bool false)
(define-data-var weighted-voting-enabled bool false)
(define-data-var minimum-quorum uint u0) ;; Minimum participation required
(define-data-var approval-threshold uint u50) ;; Percentage needed to pass (for binary votes)
(define-data-var vote-type (string-utf8 20) u"single-choice") ;; "single-choice", "multiple-choice", "ranked"
(define-data-var max-selections uint u1) ;; For multiple choice voting

;; Vote options and categories
(define-data-var vote-options (list 20 (string-utf8 50)) (list))
(define-data-var option-categories (list 20 (string-utf8 30)) (list))

;; Staking and weighting
(define-data-var minimum-stake uint u0)
(define-data-var stake-multiplier uint u1) ;; How much stake weight affects vote weight

;; Storage maps
(define-map votes principal (list 20 uint)) ;; voter -> selected options (supports multiple choice)
(define-map vote-weights principal uint) ;; voter -> vote weight (based on stake/tokens)
(define-map vote-counts uint uint) ;; option index -> vote count
(define-map weighted-vote-counts uint uint) ;; option index -> weighted vote count
(define-map used-nonces principal uint) ;; prevent replay attacks
(define-map voter-timestamps principal uint) ;; when vote was cast
(define-map vote-comments principal (string-utf8 200)) ;; optional voter comments

;; Registration and eligibility
(define-map registered-voters principal bool)
(define-map voter-eligibility principal bool)
(define-map admin-list principal bool)

;; Delegation system
(define-map vote-delegates principal principal) ;; delegator -> delegate
(define-map delegation-counts principal uint) ;; delegate -> number of delegators
(define-map delegate-votes principal (list 20 uint)) ;; delegate votes on behalf of others

;; Stake tracking
(define-map voter-stakes principal uint)
(define-map total-staked-per-option uint uint)

;; Results and analytics
(define-map voter-participation-history principal uint) ;; total votes by voter
(define-data-var total-registered-voters uint u0)
(define-data-var total-votes-cast uint u0)
(define-data-var total-weighted-votes uint u0)

;; Events (using print for logging)
(define-private (log-vote-cast (voter principal) (options (list 20 uint)) (weight uint))
  (print {event: "vote-cast", voter: voter, options: options, weight: weight, timestamp: block-height}))

(define-private (log-delegation (delegator principal) (delegate principal))
  (print {event: "delegation", delegator: delegator, delegate: delegate, timestamp: block-height}))

(define-private (log-registration (voter principal))
  (print {event: "voter-registered", voter: voter, timestamp: block-height}))

;; Domain hash for signature verification
(define-data-var domain-hash (buff 32) 0x00)

;; Initialize the enhanced voting contract
(define-public (initialize-enhanced-voting 
                (title (string-utf8 100))
                (description (string-utf8 500))
                (options (list 20 (string-utf8 50)))
                (categories (list 20 (string-utf8 30)))
                (start-height uint)
                (end-height uint)
                (config {
                  require-registration: bool,
                  allow-delegation: bool,
                  weighted-voting: bool,
                  minimum-quorum: uint,
                  approval-threshold: uint,
                  vote-type: (string-utf8 20),
                  max-selections: uint,
                  minimum-stake: uint
                }))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get voting-active)) ERR-NOT-AUTHORIZED)
    (asserts! (> end-height start-height) ERR-NOT-AUTHORIZED)
    (asserts! (> (len options) u0) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get emergency-stop)) ERR-EMERGENCY-STOP)
    
    ;; Set basic voting parameters
    (var-set voting-title title)
    (var-set voting-description description)
    (var-set vote-options options)
    (var-set option-categories categories)
    (var-set voting-start-height start-height)
    (var-set voting-end-height end-height)
    (var-set voting-active true)
    
    ;; Configure advanced features
    (var-set require-registration (get require-registration config))
    (var-set allow-vote-delegation (get allow-delegation config))
    (var-set weighted-voting-enabled (get weighted-voting config))
    (var-set minimum-quorum (get minimum-quorum config))
    (var-set approval-threshold (get approval-threshold config))
    (var-set vote-type (get vote-type config))
    (var-set max-selections (get max-selections config))
    (var-set minimum-stake (get minimum-stake config))
    
    ;; Initialize vote counts
    (map set-vote-count-to-zero (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19))
    
    ;; Set domain hash
    (var-set domain-hash (keccak256 (concat 
      (concat 
        (unwrap-panic (as-max-len? (unwrap-panic (to-consensus-buff? "ENHANCED_VOTE:")) u20))
        (unwrap-panic (as-max-len? (unwrap-panic (to-consensus-buff? title)) u100)))
      (unwrap-panic (as-max-len? (unwrap-panic (to-consensus-buff? start-height)) u8)))))
    
    ;; Register contract owner as admin and voter
    (map-set admin-list CONTRACT-OWNER true)
    (map-set voter-eligibility CONTRACT-OWNER true)
    
    (ok true)))

;; Admin functions
(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set admin-list new-admin true)
    (ok true)))

(define-public (emergency-stop-toggle)
  (begin
    (asserts! (default-to false (map-get? admin-list tx-sender)) ERR-NOT-AUTHORIZED)
    (var-set emergency-stop (not (var-get emergency-stop)))
    (ok (var-get emergency-stop))))

;; Voter registration and eligibility
(define-public (register-voter)
  (begin
    (asserts! (not (var-get emergency-stop)) ERR-EMERGENCY-STOP)
    (asserts! (var-get require-registration) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? registered-voters tx-sender)) ERR-ALREADY-VOTED)
    
    (map-set registered-voters tx-sender true)
    (map-set voter-eligibility tx-sender true)
    (var-set total-registered-voters (+ (var-get total-registered-voters) u1))
    (log-registration tx-sender)
    (ok true)))

(define-public (set-voter-eligibility (voter principal) (eligible bool))
  (begin
    (asserts! (default-to false (map-get? admin-list tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set voter-eligibility voter eligible)
    (ok true)))

;; Staking system
(define-public (stake-tokens (amount uint))
  (begin
    (asserts! (var-get weighted-voting-enabled) ERR-WEIGHTED-VOTING-DISABLED)
    (asserts! (>= amount (var-get minimum-stake)) ERR-INSUFFICIENT-STAKE)
    
    ;; In a real implementation, you'd transfer tokens to the contract
    ;; For now, we just record the stake amount
    (map-set voter-stakes tx-sender amount)
    (map-set vote-weights tx-sender (* amount (var-get stake-multiplier)))
    (ok true)))

;; Delegation system
(define-public (delegate-vote (delegate principal))
  (begin
    (asserts! (var-get allow-vote-delegation) ERR-DELEGATION-NOT-ALLOWED)
    (asserts! (not (is-eq tx-sender delegate)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? vote-delegates tx-sender)) ERR-ALREADY-DELEGATED)
    (asserts! (check-voter-eligibility tx-sender) ERR-NOT-ELIGIBLE)
    (asserts! (check-voter-eligibility delegate) ERR-NOT-ELIGIBLE)
    
    (map-set vote-delegates tx-sender delegate)
    (map-set delegation-counts delegate 
             (+ (default-to u0 (map-get? delegation-counts delegate)) u1))
    (log-delegation tx-sender delegate)
    (ok true)))

(define-public (revoke-delegation)
  (let ((current-delegate (map-get? vote-delegates tx-sender)))
    (match current-delegate
      delegate (begin
        (map-delete vote-delegates tx-sender)
        (map-set delegation-counts delegate 
                 (- (default-to u1 (map-get? delegation-counts delegate)) u1))
        (ok true))
      ERR-NOT-AUTHORIZED)))

;; Enhanced vote submission with multiple choice support
(define-public (submit-enhanced-vote 
                (selected-options (list 20 uint))
                (comment (optional (string-utf8 200))))
  (let ((current-height block-height)
        (vote-weight (calculate-vote-weight tx-sender)))
    
    (asserts! (not (var-get emergency-stop)) ERR-EMERGENCY-STOP)
    (asserts! (var-get voting-active) ERR-VOTING-CLOSED)
    (asserts! (>= current-height (var-get voting-start-height)) ERR-VOTING-NOT-STARTED)
    (asserts! (<= current-height (var-get voting-end-height)) ERR-VOTING-CLOSED)
    (asserts! (check-voter-eligibility tx-sender) ERR-NOT-ELIGIBLE)
    (asserts! (is-none (map-get? votes tx-sender)) ERR-ALREADY-VOTED)
    (asserts! (validate-vote-selection selected-options) ERR-INVALID-OPTION)
    
    ;; Record the vote
    (map-set votes tx-sender selected-options)
    (map-set voter-timestamps tx-sender current-height)
    (var-set total-votes-cast (+ (var-get total-votes-cast) u1))
    
    ;; Handle comment if provided
    (match comment
      c (map-set vote-comments tx-sender c)
      true)
    
    ;; Update vote counts
    (map update-vote-counts selected-options)
    (update-weighted-counts-for-options selected-options vote-weight)
    
    ;; Update participation history
    (map-set voter-participation-history tx-sender 
             (+ (default-to u0 (map-get? voter-participation-history tx-sender)) u1))
    
    (var-set total-weighted-votes (+ (var-get total-weighted-votes) vote-weight))
    (log-vote-cast tx-sender selected-options vote-weight)
    (ok true)))

;; Delegate voting on behalf of others
(define-public (submit-delegate-vote 
                (selected-options (list 20 uint))
                (comment (optional (string-utf8 200))))
  (let ((delegation-count (default-to u0 (map-get? delegation-counts tx-sender)))
        (vote-weight (calculate-delegate-vote-weight tx-sender)))
    
    (asserts! (var-get allow-vote-delegation) ERR-DELEGATION-NOT-ALLOWED)
    (asserts! (> delegation-count u0) ERR-NOT-AUTHORIZED)
    (asserts! (validate-vote-selection selected-options) ERR-INVALID-OPTION)
    (asserts! (is-none (map-get? delegate-votes tx-sender)) ERR-ALREADY-VOTED)
    
    ;; Record delegate vote
    (map-set delegate-votes tx-sender selected-options)
    
    ;; Update vote counts with delegation weight
    (update-counts-for-options selected-options delegation-count)
    (update-weighted-counts-for-options selected-options vote-weight)
    
    (var-set total-votes-cast (+ (var-get total-votes-cast) delegation-count))
    (var-set total-weighted-votes (+ (var-get total-weighted-votes) vote-weight))
    (ok true)))

;; Enhanced offline voting with signature verification
(define-public (submit-vote-with-enhanced-signature 
                (voter principal)
                (selected-options (list 20 uint))
                (nonce uint)
                (comment (optional (string-utf8 200)))
                (signature (buff 65)))
  (let ((current-height block-height)
        (message-hash (create-enhanced-vote-message-hash voter selected-options nonce comment))
        (recovered-pubkey (unwrap! (secp256k1-recover? message-hash signature) ERR-INVALID-SIGNATURE))
        (recovered-address (unwrap! (principal-of? recovered-pubkey) ERR-INVALID-SIGNATURE))
        (vote-weight (calculate-vote-weight voter)))
    
    ;; Standard verification
    (asserts! (not (var-get emergency-stop)) ERR-EMERGENCY-STOP)
    (asserts! (var-get voting-active) ERR-VOTING-CLOSED)
    (asserts! (>= current-height (var-get voting-start-height)) ERR-VOTING-NOT-STARTED)
    (asserts! (<= current-height (var-get voting-end-height)) ERR-VOTING-CLOSED)
    (asserts! (validate-vote-selection selected-options) ERR-INVALID-OPTION)
    (asserts! (is-eq recovered-address voter) ERR-INVALID-SIGNATURE)
    (asserts! (check-voter-eligibility voter) ERR-NOT-ELIGIBLE)
    (asserts! (is-none (map-get? votes voter)) ERR-ALREADY-VOTED)
    (asserts! (> nonce (default-to u0 (map-get? used-nonces voter))) ERR-REPLAY-ATTACK)
    
    ;; Record the vote
    (map-set votes voter selected-options)
    (map-set used-nonces voter nonce)
    (map-set voter-timestamps voter current-height)
    
    ;; Handle comment
    (match comment
      c (map-set vote-comments voter c)
      true)
    
    ;; Update counts
    (map update-vote-counts selected-options)
    (update-weighted-counts-for-options selected-options vote-weight)
    
    (var-set total-votes-cast (+ (var-get total-votes-cast) u1))
    (var-set total-weighted-votes (+ (var-get total-weighted-votes) vote-weight))
    (log-vote-cast voter selected-options vote-weight)
    (ok true)))

;; Finalize voting and determine winner
(define-public (finalize-voting)
  (let ((total-participation (var-get total-votes-cast))
        (required-quorum (var-get minimum-quorum))
        (current-height block-height))
    
    (asserts! (default-to false (map-get? admin-list tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get voting-finalized)) ERR-ALREADY-FINALIZED)
    (asserts! (or (not (var-get voting-active)) 
                  (>= current-height (var-get voting-end-height))) ERR-VOTING-NOT-FINALIZED)
    
    ;; Check quorum if required
    (if (> required-quorum u0)
        (asserts! (>= total-participation required-quorum) ERR-INVALID-QUORUM)
        true)
    
    ;; Determine winner based on vote type
    (var-set winning-option (determine-winner))
    (var-set voting-finalized true)
    (var-set voting-active false)
    
    (print {event: "voting-finalized", winner: (var-get winning-option), 
            total-votes: total-participation, timestamp: current-height})
    (ok (var-get winning-option))))

;; Helper functions

(define-private (set-vote-count-to-zero (index uint))
  (begin
    (map-set vote-counts index u0)
    (map-set weighted-vote-counts index u0)))

(define-private (check-voter-eligibility (voter principal))
  (if (var-get require-registration)
      (and (default-to false (map-get? registered-voters voter))
           (default-to false (map-get? voter-eligibility voter)))
      (default-to true (map-get? voter-eligibility voter))))

(define-private (calculate-vote-weight (voter principal))
  (if (var-get weighted-voting-enabled)
      (default-to u1 (map-get? vote-weights voter))
      u1))

(define-private (calculate-delegate-vote-weight (delegate principal))
  (let ((delegation-count (default-to u0 (map-get? delegation-counts delegate)))
        (delegate-weight (calculate-vote-weight delegate)))
    (* delegation-count delegate-weight)))

(define-private (validate-vote-selection (options (list 20 uint)))
  (let ((option-count (len options))
        (max-options (len (var-get vote-options)))
        (max-allowed (var-get max-selections)))
    (and (<= option-count max-allowed)
         (> option-count u0)
         (fold validate-single-option options true))))

(define-private (validate-single-option (option uint) (acc bool))
  (and acc (< option (len (var-get vote-options)))))

(define-private (update-vote-counts (option uint))
  (map-set vote-counts option 
           (+ (default-to u0 (map-get? vote-counts option)) u1)))

(define-private (update-vote-counts-with-multiplier (option uint) (multiplier uint))
  (map-set vote-counts option 
           (+ (default-to u0 (map-get? vote-counts option)) multiplier)))

(define-private (update-weighted-vote-counts-with-weight (option uint) (weight uint))
  (map-set weighted-vote-counts option 
           (+ (default-to u0 (map-get? weighted-vote-counts option)) weight)))

;; Helper functions to update multiple options with weights
(define-private (update-weighted-counts-for-options (options (list 20 uint)) (weight uint))
  (fold update-weighted-count-folder options weight))

(define-private (update-weighted-count-folder (option uint) (weight uint))
  (begin
    (update-weighted-vote-counts-with-weight option weight)
    weight))

(define-private (update-counts-for-options (options (list 20 uint)) (multiplier uint))
  (fold update-count-folder options multiplier))

(define-private (update-count-folder (option uint) (multiplier uint))
  (begin
    (update-vote-counts-with-multiplier option multiplier)
    multiplier))

(define-private (determine-winner)
  (let ((vote-type-str (var-get vote-type)))
    (if (is-eq vote-type-str u"single-choice")
        (get-highest-voted-option)
        (if (is-eq vote-type-str u"multiple-choice")
            (get-highest-voted-option)
            (get-highest-voted-option))))) ;; Default to highest for now

(define-private (get-highest-voted-option)
  (fold find-max-vote 
        (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)
        u0))

(define-private (find-max-vote (option uint) (current-max uint))
  (if (> (default-to u0 (map-get? weighted-vote-counts option))
         (default-to u0 (map-get? weighted-vote-counts current-max)))
      option
      current-max))

(define-private (create-enhanced-vote-message-hash 
                (voter principal) 
                (options (list 20 uint)) 
                (nonce uint)
                (comment (optional (string-utf8 200))))
  (keccak256 (concat 
    (concat 
      (concat 
        (concat 
          (var-get domain-hash) 
          (unwrap-panic (as-max-len? (unwrap-panic (to-consensus-buff? voter)) u20)))
        (unwrap-panic (as-max-len? (unwrap-panic (to-consensus-buff? options)) u160)))
      (unwrap-panic (as-max-len? (unwrap-panic (to-consensus-buff? nonce)) u8)))
    (unwrap-panic (as-max-len? (unwrap-panic (to-consensus-buff? comment)) u208)))))

;; Read-only functions

(define-read-only (get-comprehensive-voting-info)
  {
    basic: {
      active: (var-get voting-active),
      title: (var-get voting-title),
      description: (var-get voting-description),
      start-height: (var-get voting-start-height),
      end-height: (var-get voting-end-height),
      current-height: block-height,
      finalized: (var-get voting-finalized),
      winner: (var-get winning-option)
    },
    config: {
      require-registration: (var-get require-registration),
      allow-delegation: (var-get allow-vote-delegation),
      weighted-voting: (var-get weighted-voting-enabled),
      minimum-quorum: (var-get minimum-quorum),
      approval-threshold: (var-get approval-threshold),
      vote-type: (var-get vote-type),
      max-selections: (var-get max-selections),
      minimum-stake: (var-get minimum-stake)
    },
    options: (var-get vote-options),
    categories: (var-get option-categories),
    stats: {
      total-registered: (var-get total-registered-voters),
      total-votes-cast: (var-get total-votes-cast),
      total-weighted-votes: (var-get total-weighted-votes),
      emergency-stop: (var-get emergency-stop)
    }
  })

(define-read-only (get-vote-results)
  {
    regular-counts: (map get-vote-count (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)),
    weighted-counts: (map get-weighted-vote-count (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)),
    winner: (var-get winning-option),
    finalized: (var-get voting-finalized)
  })

(define-read-only (get-voter-info (voter principal))
  {
    eligible: (check-voter-eligibility voter),
    registered: (default-to false (map-get? registered-voters voter)),
    has-voted: (is-some (map-get? votes voter)),
    vote-selections: (map-get? votes voter),
    vote-weight: (calculate-vote-weight voter),
    stake: (default-to u0 (map-get? voter-stakes voter)),
    delegation-count: (default-to u0 (map-get? delegation-counts voter)),
    delegated-to: (map-get? vote-delegates voter),
    comment: (map-get? vote-comments voter),
    vote-timestamp: (map-get? voter-timestamps voter),
    participation-history: (default-to u0 (map-get? voter-participation-history voter))
  })

(define-read-only (get-vote-count (option-index uint))
  (default-to u0 (map-get? vote-counts option-index)))

(define-read-only (get-weighted-vote-count (option-index uint))
  (default-to u0 (map-get? weighted-vote-counts option-index)))

(define-read-only (get-quorum-status)
  {
    required: (var-get minimum-quorum),
    current: (var-get total-votes-cast),
    percentage: (if (> (var-get total-registered-voters) u0)
                    (/ (* (var-get total-votes-cast) u100) (var-get total-registered-voters))
                    u0),
    met: (>= (var-get total-votes-cast) (var-get minimum-quorum))
  })

(define-read-only (get-enhanced-message-hash 
                  (voter principal) 
                  (options (list 20 uint)) 
                  (nonce uint)
                  (comment (optional (string-utf8 200))))
  (create-enhanced-vote-message-hash voter options nonce comment))

(define-read-only (is-admin (user principal))
  (default-to false (map-get? admin-list user)))

(define-read-only (get-delegation-info (delegate principal))
  {
    delegation-count: (default-to u0 (map-get? delegation-counts delegate)),
    has-voted-as-delegate: (is-some (map-get? delegate-votes delegate)),
    delegate-selections: (map-get? delegate-votes delegate)
  })