(define-trait workstamp-trait
  (
    (get-workstamp (uint) (response (optional {employer: principal, employee: principal, role: (string-ascii 50), start-block: uint, end-block: (optional uint), verified: bool}) uint))
  )
)

(define-non-fungible-token workstamp uint)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_WORKSTAMP_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_EMPLOYEE (err u103))
(define-constant ERR_WORKSTAMP_ACTIVE (err u104))
(define-constant ERR_NOT_EMPLOYER (err u105))
(define-constant ERR_INVALID_ROLE (err u106))
(define-constant ERR_INVALID_DATES (err u107))
(define-constant ERR_SKILL_NOT_FOUND (err u108))
(define-constant ERR_INVALID_RATING (err u109))
(define-constant ERR_ALREADY_ENDORSED (err u110))
(define-constant ERR_CANNOT_ENDORSE_SELF (err u111))
(define-constant ERR_EMPLOYMENT_ENDED (err u112))
(define-constant ERR_DISPUTE_NOT_FOUND (err u113))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u114))
(define-constant ERR_INVALID_DISPUTE_TYPE (err u115))
(define-constant ERR_DISPUTE_ALREADY_RESOLVED (err u116))
(define-constant ERR_NOT_DISPUTE_PARTICIPANT (err u117))
(define-constant ERR_INVALID_EVIDENCE (err u118))
(define-constant ERR_CANNOT_RESOLVE_OWN_DISPUTE (err u119))

(define-data-var next-workstamp-id uint u1)
(define-data-var contract-paused bool false)
(define-data-var next-endorsement-id uint u1)
(define-data-var next-dispute-id uint u1)

(define-map workstamps
  uint
  {
    employer: principal,
    employee: principal,
    role: (string-ascii 50),
    start-block: uint,
    end-block: (optional uint),
    verified: bool,
    created-at: uint
  }
)

(define-map employee-workstamps
  principal
  (list 100 uint)
)

(define-map employer-workstamps
  principal
  (list 100 uint)
)

(define-map active-employment
  {employer: principal, employee: principal}
  uint
)

(define-map skill-endorsements
  uint
  {
    workstamp-id: uint,
    endorser: principal,
    employee: principal,
    skill-name: (string-ascii 50),
    rating: uint,
    comments: (string-ascii 200),
    endorsed-at: uint
  }
)

(define-map employee-endorsements
  principal
  (list 500 uint)
)

(define-map workstamp-endorsements
  uint
  (list 50 uint)
)

(define-map skill-ratings
  {employee: principal, skill-name: (string-ascii 50)}
  {
    total-rating: uint,
    endorsement-count: uint,
    average-rating: uint
  }
)

(define-map endorsement-exists
  {workstamp-id: uint, endorser: principal, skill-name: (string-ascii 50)}
  bool
)

(define-map disputes
  uint
  {
    workstamp-id: uint,
    disputant: principal,
    respondent: principal,
    dispute-type: uint,
    reason: (string-ascii 200),
    evidence-hash: (string-ascii 64),
    status: uint,
    resolution: (optional (string-ascii 300)),
    resolved-by: (optional principal),
    created-at: uint,
    resolved-at: (optional uint)
  }
)

(define-map workstamp-disputes
  uint
  (list 10 uint)
)

(define-map participant-disputes
  principal
  (list 50 uint)
)

(define-map dispute-evidence
  uint
  (list 5 (string-ascii 64))
)

(define-map dispute-votes
  {dispute-id: uint, voter: principal}
  {vote: bool, voted-at: uint}
)

(define-map dispute-vote-summary
  uint
  {
    total-votes: uint,
    support-votes: uint,
    against-votes: uint
  }
)

(define-public (issue-workstamp (employee principal) (role (string-ascii 50)))
  (let
    (
      (workstamp-id (var-get next-workstamp-id))
      (current-block stacks-block-height)
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (> (len role) u0) ERR_INVALID_ROLE)
    (asserts! (not (is-eq tx-sender employee)) ERR_INVALID_EMPLOYEE)
    (asserts! (is-none (map-get? active-employment {employer: tx-sender, employee: employee})) ERR_ALREADY_EXISTS)
    
    (try! (nft-mint? workstamp workstamp-id employee))
    
    (map-set workstamps workstamp-id
      {
        employer: tx-sender,
        employee: employee,
        role: role,
        start-block: current-block,
        end-block: none,
        verified: false,
        created-at: current-block
      }
    )
    
    (map-set active-employment {employer: tx-sender, employee: employee} workstamp-id)
    
    (map-set employee-workstamps employee
      (unwrap! (as-max-len? (append (default-to (list) (map-get? employee-workstamps employee)) workstamp-id) u100) ERR_NOT_AUTHORIZED)
    )
    
    (map-set employer-workstamps tx-sender
      (unwrap! (as-max-len? (append (default-to (list) (map-get? employer-workstamps tx-sender)) workstamp-id) u100) ERR_NOT_AUTHORIZED)
    )
    
    (var-set next-workstamp-id (+ workstamp-id u1))
    (ok workstamp-id)
  )
)

(define-public (end-employment (workstamp-id uint))
  (let
    (
      (ws (unwrap! (map-get? workstamps workstamp-id) ERR_WORKSTAMP_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender (get employer ws)) ERR_NOT_EMPLOYER)
    (asserts! (is-none (get end-block ws)) ERR_WORKSTAMP_ACTIVE)
    
    (map-set workstamps workstamp-id
      (merge ws {end-block: (some current-block)})
    )
    
    (map-delete active-employment {employer: (get employer ws), employee: (get employee ws)})
    (ok true)
  )
)

(define-public (verify-workstamp (workstamp-id uint))
  (let
    (
      (ws (unwrap! (map-get? workstamps workstamp-id) ERR_WORKSTAMP_NOT_FOUND))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender (get employee ws)) ERR_NOT_AUTHORIZED)
    
    (map-set workstamps workstamp-id
      (merge ws {verified: true})
    )
    (ok true)
  )
)

(define-public (update-role (workstamp-id uint) (new-role (string-ascii 50)))
  (let
    (
      (ws (unwrap! (map-get? workstamps workstamp-id) ERR_WORKSTAMP_NOT_FOUND))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender (get employer ws)) ERR_NOT_EMPLOYER)
    (asserts! (> (len new-role) u0) ERR_INVALID_ROLE)
    (asserts! (is-none (get end-block ws)) ERR_WORKSTAMP_ACTIVE)
    
    (map-set workstamps workstamp-id
      (merge ws {role: new-role})
    )
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

(define-read-only (get-workstamp (workstamp-id uint))
  (ok (map-get? workstamps workstamp-id))
)

(define-read-only (get-employee-workstamps (employee principal))
  (ok (default-to (list) (map-get? employee-workstamps employee)))
)

(define-read-only (get-employer-workstamps (employer principal))
  (ok (default-to (list) (map-get? employer-workstamps employer)))
)

(define-read-only (get-active-employment (employer principal) (employee principal))
  (ok (map-get? active-employment {employer: employer, employee: employee}))
)

(define-read-only (is-employment-active (workstamp-id uint))
  (match (map-get? workstamps workstamp-id)
    ws (ok (is-none (get end-block ws)))
    (ok false)
  )
)

(define-read-only (get-workstamp-duration (workstamp-id uint))
  (match (map-get? workstamps workstamp-id)
    ws 
      (ok (match (get end-block ws)
        end-block (- end-block (get start-block ws))
        (- stacks-block-height (get start-block ws))
      ))
    ERR_WORKSTAMP_NOT_FOUND
  )
)

(define-read-only (get-next-workstamp-id)
  (ok (var-get next-workstamp-id))
)

(define-read-only (is-contract-paused)
  (ok (var-get contract-paused))
)

(define-read-only (get-workstamp-owner (workstamp-id uint))
  (ok (nft-get-owner? workstamp workstamp-id))
)

(define-read-only (validate-workstamp (workstamp-id uint) (expected-employer principal) (expected-employee principal))
  (match (map-get? workstamps workstamp-id)
    ws 
      (ok (and 
        (is-eq (get employer ws) expected-employer)
        (is-eq (get employee ws) expected-employee)
        (get verified ws)
      ))
    (ok false)
  )
)

(define-read-only (get-employee-verified-workstamps (employee principal))
  (let
    (
      (workstamp-ids (default-to (list) (map-get? employee-workstamps employee)))
    )
    (ok (filter is-workstamp-verified workstamp-ids))
  )
)

(define-private (is-workstamp-verified (workstamp-id uint))
  (match (map-get? workstamps workstamp-id)
    ws (get verified ws)
    false
  )
)

(define-read-only (get-contract-stats)
  (ok {
    total-workstamps: (- (var-get next-workstamp-id) u1),
    contract-paused: (var-get contract-paused),
    contract-owner: CONTRACT_OWNER
  })
)

(define-public (endorse-skill (workstamp-id uint) (skill-name (string-ascii 50)) (rating uint) (comments (string-ascii 200)))
  (let
    (
      (ws (unwrap! (map-get? workstamps workstamp-id) ERR_WORKSTAMP_NOT_FOUND))
      (endorsement-id (var-get next-endorsement-id))
      (current-block stacks-block-height)
      (employee (get employee ws))
      (endorsement-key {workstamp-id: workstamp-id, endorser: tx-sender, skill-name: skill-name})
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender (get employer ws)) ERR_NOT_EMPLOYER)
    (asserts! (not (is-eq tx-sender employee)) ERR_CANNOT_ENDORSE_SELF)
    (asserts! (> (len skill-name) u0) ERR_INVALID_ROLE)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (is-none (get end-block ws)) ERR_EMPLOYMENT_ENDED)
    (asserts! (is-none (map-get? endorsement-exists endorsement-key)) ERR_ALREADY_ENDORSED)
    
    (map-set skill-endorsements endorsement-id
      {
        workstamp-id: workstamp-id,
        endorser: tx-sender,
        employee: employee,
        skill-name: skill-name,
        rating: rating,
        comments: comments,
        endorsed-at: current-block
      }
    )
    
    (map-set endorsement-exists endorsement-key true)
    
    (map-set employee-endorsements employee
      (unwrap! (as-max-len? (append (default-to (list) (map-get? employee-endorsements employee)) endorsement-id) u500) ERR_NOT_AUTHORIZED)
    )
    
    (map-set workstamp-endorsements workstamp-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? workstamp-endorsements workstamp-id)) endorsement-id) u50) ERR_NOT_AUTHORIZED)
    )
    
    (let
      (
        (skill-key {employee: employee, skill-name: skill-name})
        (current-skill-data (default-to {total-rating: u0, endorsement-count: u0, average-rating: u0} (map-get? skill-ratings skill-key)))
        (new-total-rating (+ (get total-rating current-skill-data) rating))
        (new-count (+ (get endorsement-count current-skill-data) u1))
        (new-average (/ new-total-rating new-count))
      )
      (map-set skill-ratings skill-key
        {
          total-rating: new-total-rating,
          endorsement-count: new-count,
          average-rating: new-average
        }
      )
    )
    
    (var-set next-endorsement-id (+ endorsement-id u1))
    (ok endorsement-id)
  )
)

(define-public (update-endorsement (endorsement-id uint) (new-rating uint) (new-comments (string-ascii 200)))
  (let
    (
      (endorsement (unwrap! (map-get? skill-endorsements endorsement-id) ERR_SKILL_NOT_FOUND))
      (workstamp-id (get workstamp-id endorsement))
      (ws (unwrap! (map-get? workstamps workstamp-id) ERR_WORKSTAMP_NOT_FOUND))
      (employee (get employee endorsement))
      (skill-name (get skill-name endorsement))
      (old-rating (get rating endorsement))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender (get endorser endorsement)) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= new-rating u1) (<= new-rating u5)) ERR_INVALID_RATING)
    (asserts! (is-none (get end-block ws)) ERR_EMPLOYMENT_ENDED)
    
    (map-set skill-endorsements endorsement-id
      (merge endorsement {rating: new-rating, comments: new-comments})
    )
    
    (let
      (
        (skill-key {employee: employee, skill-name: skill-name})
        (current-skill-data (unwrap! (map-get? skill-ratings skill-key) ERR_SKILL_NOT_FOUND))
        (adjusted-total-rating (+ (- (get total-rating current-skill-data) old-rating) new-rating))
        (count (get endorsement-count current-skill-data))
        (new-average (/ adjusted-total-rating count))
      )
      (map-set skill-ratings skill-key
        {
          total-rating: adjusted-total-rating,
          endorsement-count: count,
          average-rating: new-average
        }
      )
    )
    
    (ok true)
  )
)

(define-read-only (get-endorsement (endorsement-id uint))
  (ok (map-get? skill-endorsements endorsement-id))
)

(define-read-only (get-employee-endorsements (employee principal))
  (ok (default-to (list) (map-get? employee-endorsements employee)))
)

(define-read-only (get-workstamp-endorsements (workstamp-id uint))
  (ok (default-to (list) (map-get? workstamp-endorsements workstamp-id)))
)

(define-read-only (get-skill-rating (employee principal) (skill-name (string-ascii 50)))
  (ok (map-get? skill-ratings {employee: employee, skill-name: skill-name}))
)

(define-read-only (get-employee-top-skills (employee principal))
  (let
    (
      (endorsement-ids (default-to (list) (map-get? employee-endorsements employee)))
    )
    (ok (get-top-skills-from-endorsements endorsement-ids))
  )
)

(define-private (get-top-skills-from-endorsements (endorsement-ids (list 500 uint)))
  (fold extract-skill-from-endorsement endorsement-ids (list))
)

(define-private (extract-skill-from-endorsement (endorsement-id uint) (acc (list 100 {skill: (string-ascii 50), rating: uint})))
  (match (map-get? skill-endorsements endorsement-id)
    endorsement 
      (let
        (
          (skill-name (get skill-name endorsement))
          (rating (get rating endorsement))
          (skill-entry {skill: skill-name, rating: rating})
        )
        (unwrap-panic (as-max-len? (append acc skill-entry) u100))
      )
    acc
  )
)

(define-read-only (has-skill-endorsement (workstamp-id uint) (endorser principal) (skill-name (string-ascii 50)))
  (ok (is-some (map-get? endorsement-exists {workstamp-id: workstamp-id, endorser: endorser, skill-name: skill-name})))
)

(define-read-only (get-endorsement-stats)
  (ok {
    total-endorsements: (- (var-get next-endorsement-id) u1),
    next-endorsement-id: (var-get next-endorsement-id)
  })
)

(define-public (create-dispute (workstamp-id uint) (dispute-type uint) (reason (string-ascii 200)) (evidence-hash (string-ascii 64)))
  (let
    (
      (ws (unwrap! (map-get? workstamps workstamp-id) ERR_WORKSTAMP_NOT_FOUND))
      (dispute-id (var-get next-dispute-id))
      (current-block stacks-block-height)
      (disputant tx-sender)
      (respondent (if (is-eq tx-sender (get employer ws)) (get employee ws) (get employer ws)))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq tx-sender (get employer ws)) (is-eq tx-sender (get employee ws))) ERR_NOT_DISPUTE_PARTICIPANT)
    (asserts! (<= dispute-type u3) ERR_INVALID_DISPUTE_TYPE)
    (asserts! (> (len reason) u0) ERR_INVALID_EVIDENCE)
    (asserts! (> (len evidence-hash) u0) ERR_INVALID_EVIDENCE)
    (asserts! (is-none (get-active-dispute-for-workstamp workstamp-id)) ERR_DISPUTE_ALREADY_EXISTS)
    
    (map-set disputes dispute-id
      {
        workstamp-id: workstamp-id,
        disputant: disputant,
        respondent: respondent,
        dispute-type: dispute-type,
        reason: reason,
        evidence-hash: evidence-hash,
        status: u1,
        resolution: none,
        resolved-by: none,
        created-at: current-block,
        resolved-at: none
      }
    )
    
    (map-set workstamp-disputes workstamp-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? workstamp-disputes workstamp-id)) dispute-id) u10) ERR_NOT_AUTHORIZED)
    )
    
    (map-set participant-disputes disputant
      (unwrap! (as-max-len? (append (default-to (list) (map-get? participant-disputes disputant)) dispute-id) u50) ERR_NOT_AUTHORIZED)
    )
    
    (map-set participant-disputes respondent
      (unwrap! (as-max-len? (append (default-to (list) (map-get? participant-disputes respondent)) dispute-id) u50) ERR_NOT_AUTHORIZED)
    )
    
    (map-set dispute-evidence dispute-id (list evidence-hash))
    
    (map-set dispute-vote-summary dispute-id
      {
        total-votes: u0,
        support-votes: u0,
        against-votes: u0
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (add-dispute-evidence (dispute-id uint) (evidence-hash (string-ascii 64)))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
      (current-evidence (default-to (list) (map-get? dispute-evidence dispute-id)))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq tx-sender (get disputant dispute)) (is-eq tx-sender (get respondent dispute))) ERR_NOT_DISPUTE_PARTICIPANT)
    (asserts! (is-eq (get status dispute) u1) ERR_DISPUTE_ALREADY_RESOLVED)
    (asserts! (> (len evidence-hash) u0) ERR_INVALID_EVIDENCE)
    (asserts! (< (len current-evidence) u5) ERR_NOT_AUTHORIZED)
    
    (map-set dispute-evidence dispute-id
      (unwrap! (as-max-len? (append current-evidence evidence-hash) u5) ERR_NOT_AUTHORIZED)
    )
    
    (ok true)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (support bool))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
      (current-block stacks-block-height)
      (vote-key {dispute-id: dispute-id, voter: tx-sender})
      (current-summary (default-to {total-votes: u0, support-votes: u0, against-votes: u0} (map-get? dispute-vote-summary dispute-id)))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute) u1) ERR_DISPUTE_ALREADY_RESOLVED)
    (asserts! (not (is-eq tx-sender (get disputant dispute))) ERR_CANNOT_RESOLVE_OWN_DISPUTE)
    (asserts! (not (is-eq tx-sender (get respondent dispute))) ERR_CANNOT_RESOLVE_OWN_DISPUTE)
    (asserts! (is-none (map-get? dispute-votes vote-key)) ERR_ALREADY_EXISTS)
    
    (map-set dispute-votes vote-key
      {
        vote: support,
        voted-at: current-block
      }
    )
    
    (map-set dispute-vote-summary dispute-id
      {
        total-votes: (+ (get total-votes current-summary) u1),
        support-votes: (+ (get support-votes current-summary) (if support u1 u0)),
        against-votes: (+ (get against-votes current-summary) (if support u0 u1))
      }
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint) (resolution (string-ascii 300)) (favor-disputant bool))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
      (current-block stacks-block-height)
      (vote-summary (default-to {total-votes: u0, support-votes: u0, against-votes: u0} (map-get? dispute-vote-summary dispute-id)))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute) u1) ERR_DISPUTE_ALREADY_RESOLVED)
    (asserts! (> (len resolution) u0) ERR_INVALID_EVIDENCE)
    (asserts! (>= (get total-votes vote-summary) u3) ERR_NOT_AUTHORIZED)
    
    (map-set disputes dispute-id
      (merge dispute {
        status: (if favor-disputant u2 u3),
        resolution: (some resolution),
        resolved-by: (some tx-sender),
        resolved-at: (some current-block)
      })
    )
    
    (ok true)
  )
)

(define-public (auto-resolve-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
      (current-block stacks-block-height)
      (vote-summary (default-to {total-votes: u0, support-votes: u0, against-votes: u0} (map-get? dispute-vote-summary dispute-id)))
      (support-percentage (if (> (get total-votes vote-summary) u0) (/ (* (get support-votes vote-summary) u100) (get total-votes vote-summary)) u0))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute) u1) ERR_DISPUTE_ALREADY_RESOLVED)
    (asserts! (>= (get total-votes vote-summary) u10) ERR_NOT_AUTHORIZED)
    (asserts! (> (- current-block (get created-at dispute)) u1008) ERR_NOT_AUTHORIZED)
    
    (map-set disputes dispute-id
      (merge dispute {
        status: (if (>= support-percentage u60) u2 u3),
        resolution: (some "Auto-resolved based on community vote"),
        resolved-by: none,
        resolved-at: (some current-block)
      })
    )
    
    (ok true)
  )
)

(define-read-only (get-dispute (dispute-id uint))
  (ok (map-get? disputes dispute-id))
)

(define-read-only (get-workstamp-disputes (workstamp-id uint))
  (ok (default-to (list) (map-get? workstamp-disputes workstamp-id)))
)

(define-read-only (get-participant-disputes (participant principal))
  (ok (default-to (list) (map-get? participant-disputes participant)))
)

(define-read-only (get-dispute-evidence (dispute-id uint))
  (ok (default-to (list) (map-get? dispute-evidence dispute-id)))
)

(define-read-only (get-dispute-vote-summary (dispute-id uint))
  (ok (map-get? dispute-vote-summary dispute-id))
)

(define-read-only (get-dispute-vote (dispute-id uint) (voter principal))
  (ok (map-get? dispute-votes {dispute-id: dispute-id, voter: voter}))
)

(define-private (get-active-dispute-for-workstamp (workstamp-id uint))
  (let
    (
      (dispute-ids (default-to (list) (map-get? workstamp-disputes workstamp-id)))
    )
    (fold check-active-dispute dispute-ids none)
  )
)

(define-private (check-active-dispute (dispute-id uint) (acc (optional uint)))
  (if (is-some acc)
    acc
    (match (map-get? disputes dispute-id)
      dispute (if (is-eq (get status dispute) u1) (some dispute-id) none)
      none
    )
  )
)

(define-read-only (get-dispute-stats)
  (ok {
    total-disputes: (- (var-get next-dispute-id) u1),
    next-dispute-id: (var-get next-dispute-id)
  })
)


