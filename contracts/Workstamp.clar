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

(define-data-var next-workstamp-id uint u1)
(define-data-var contract-paused bool false)

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