;; === TRAITS ===
(define-trait ft-trait
  ((transfer (uint principal principal) (response bool uint))
   (balance-of (principal) (response uint uint))
   (total-supply () (response uint uint))))

;; === HELPER FUNCTIONS ===
(define-read-only (get-block-height)
  burn-block-height)

;; === CONSTANTS ===
(define-constant DAO-QUORUM u3)
(define-constant MIN-STAKING-AMOUNT u50000)
(define-constant REFUND-DEADLINE-BLOCKS u1000)

;; Error codes
(define-constant ERR-NOT-FOUND u100)
(define-constant ERR-INSUFFICIENT-STAKE u101)
(define-constant ERR-UNAUTHORIZED u103)
(define-constant ERR-NOT-FREELANCER u104)
(define-constant ERR-ALREADY-VOTED u105)
(define-constant ERR-PROJECT-NOT-FOUND u200)
(define-constant ERR-DEADLINE-PASSED u201)
(define-constant ERR-ALREADY-VOTED-PROJECT u202)
(define-constant ERR-REFUND-CONDITION u203)
(define-constant ERR-BID-TOO-LOW u301)
(define-constant ERR-AUCTION-ENDED u302)
(define-constant ERR-AUCTION-NOT-FOUND u303)

;; === GLOBAL STATE ===
(define-data-var job-id-counter uint u0)
(define-data-var project-id-counter uint u0)

(define-map reputation principal int)
(define-map staked principal uint)
(define-map subscribers principal bool)

;; Jobs & Projects
(define-map jobs
  uint
  {
    client: principal,
    freelancer: (optional principal),
    amount: uint,
    milestone: uint,
    paid: bool,
    approved: bool
  }
)

(define-map projects
  uint
  {
    creator: principal,
    goal: uint,
    pledged: uint,
    deadline: uint,
    milestone: uint,
    funded: bool,
    failed: bool
  }
)

(define-map pledges
  { project-id: uint, backer: principal }
  uint
)

;; Voting, Auctions
(define-map votes uint (list 10 principal))
(define-map project-votes uint (list 10 principal))

(define-map job-auctions uint { min-bid: uint, highest-bidder: (optional principal), end-block: uint })

;; === SUBSCRIPTION/STAKING ===
(define-public (subscribe)
  (begin
    (map-set subscribers tx-sender true)
    (ok true)
  )
)

(define-public (stake)
  (let ((amount (stx-get-balance tx-sender)))
    (if (< amount MIN-STAKING-AMOUNT)
      (err ERR-INSUFFICIENT-STAKE)
      (begin
        (try! (stx-transfer? MIN-STAKING-AMOUNT tx-sender (as-contract tx-sender)))
        (ok (map-set staked tx-sender MIN-STAKING-AMOUNT))
      )
    )
  )
)

;; === JOB FUNCTIONS ===

(define-public (post-job (amount uint))
  (let 
    ((id (+ (var-get job-id-counter) u1))
     (job-data {
        client: tx-sender,
        freelancer: none,
        amount: amount,
        milestone: u0,
        paid: false,
        approved: false
      }))
    (begin
      (var-set job-id-counter id)
      (map-set jobs id job-data)
      (ok id)
    )
  )
)

(define-public (bid-job (job-id uint))
  (let 
    ((existing-job (unwrap! (map-get? jobs job-id) (err ERR-NOT-FOUND))))
    (asserts! (is-none (get freelancer existing-job)) (err ERR-UNAUTHORIZED))
    (ok 
      (map-set jobs 
        job-id
        (merge existing-job { freelancer: (some tx-sender) })))
  ))

(define-public (deposit-escrow (job-id uint))
  (let ((job (map-get? jobs job-id)))
    (match job job-data
      (if (is-eq tx-sender (get client job-data))
        (begin
          (try! (stx-transfer? (get amount job-data) tx-sender (as-contract tx-sender)))
          (ok true)
        )
        (err ERR-UNAUTHORIZED)
      )
      (err ERR-NOT-FOUND)
    )
  )
)

(define-public (submit-milestone (job-id uint))
  (let 
    ((existing-job (unwrap! (map-get? jobs job-id) (err ERR-NOT-FOUND)))
     (current-milestone (get milestone existing-job)))
    (asserts! 
      (is-eq (some tx-sender) (get freelancer existing-job))
      (err ERR-NOT-FREELANCER))
    (ok 
      (map-set jobs 
        job-id
        (merge existing-job { milestone: (+ u1 current-milestone) })))
  ))

(define-public (vote-approve-job (job-id uint))
  (let ((job (map-get? jobs job-id)))
    (match job job-data
      (let ((voters (default-to (list) (map-get? votes job-id))))
        (if (is-some (index-of voters tx-sender))
          (err ERR-ALREADY-VOTED)
          (let ((new-votes (unwrap-panic (as-max-len? (concat voters (list tx-sender)) u10))))
            (begin
              (map-set votes job-id new-votes)
              (if (>= (len new-votes) DAO-QUORUM)
                (begin
                  (try! (stx-transfer? (get amount job-data) (as-contract tx-sender) (unwrap-panic (get freelancer job-data))))
                  (map-set jobs job-id (merge job-data { paid: true, approved: true }))
                  (ok true)
                )
                (ok false)
              )
            )
          )
        )
      )
      (err ERR-NOT-FOUND)
    )
  )
)

;; === PROJECT CROWDFUNDING ===

(define-public (create-project (goal uint) (deadline uint))
  (let 
    ((id (+ (var-get project-id-counter) u1))
     (project-data {
        creator: tx-sender,
        goal: goal,
        pledged: u0,
        deadline: deadline,
        milestone: u0,
        funded: false,
        failed: false
      }))
    (begin
      (var-set project-id-counter id)
      (asserts! (> goal u0) (err ERR-UNAUTHORIZED))
      (asserts! (> deadline (get-block-height)) (err ERR-DEADLINE-PASSED))
      (ok (map-set projects id project-data))
    )
  ))

(define-public (pledge (project-id uint) (amount uint))
  (let 
    ((project (unwrap! (map-get? projects project-id) (err ERR-PROJECT-NOT-FOUND)))
     (deadline-height (get deadline project)))
    (asserts! (<= (get-block-height) deadline-height) (err ERR-DEADLINE-PASSED))
    (asserts! (> amount u0) (err ERR-UNAUTHORIZED))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set pledges 
      { project-id: project-id, backer: tx-sender }
      (+ (default-to u0 (map-get? pledges { project-id: project-id, backer: tx-sender })) amount))
    (ok (map-set projects 
         project-id
         (merge project { pledged: (+ (get pledged project) amount) })))))

(define-public (vote-project-release (project-id uint))
  (let ((project (map-get? projects project-id)))
    (match project p
      (let ((current-votes (default-to (list) (map-get? project-votes project-id))))
        (if (is-some (index-of current-votes tx-sender))
          (err ERR-ALREADY-VOTED-PROJECT)
          (let ((updated (unwrap-panic (as-max-len? (concat current-votes (list tx-sender)) u10))))
            (begin
              (map-set project-votes project-id updated)
              (if (>= (len updated) DAO-QUORUM)
                (begin
                  (try! (stx-transfer? (/ (get pledged p) u3) (as-contract tx-sender) (get creator p)))
                  (map-set projects project-id (merge p { milestone: (+ (get milestone p) u1), funded: true }))
                  (ok true)
                )
                (ok false)
              )
            )
          )
        )
      )
      (err ERR-PROJECT-NOT-FOUND)
    )
  )
)

(define-public (refund (project-id uint))
  (let
    ((project (unwrap! (map-get? projects project-id) (err ERR-PROJECT-NOT-FOUND)))
     (pledged-amount (unwrap! (map-get? pledges { project-id: project-id, backer: tx-sender }) 
                             (err ERR-NOT-FOUND))))
    (asserts! (> (get-block-height) (get deadline project)) (err ERR-REFUND-CONDITION))
    (asserts! (not (get funded project)) (err ERR-REFUND-CONDITION))
    (map-delete pledges { project-id: project-id, backer: tx-sender })
    (try! (stx-transfer? pledged-amount (as-contract tx-sender) tx-sender))
    (ok (map-set projects project-id 
         (merge project { failed: true })))))

;; === REPUTATION SYSTEM ===

(define-public (rate-user (user principal) (score int))
  (begin
    (asserts! (and (>= score (- 0 5)) (<= score 5)) (err ERR-UNAUTHORIZED))
    (let 
      ((current-rep (default-to 0 (map-get? reputation user)))
       (new-score (+ current-rep score)))
      (asserts! (and (>= new-score (- 0 100)) (<= new-score 100)) (err ERR-UNAUTHORIZED))
      (ok (map-set reputation user new-score)))
  ))

;; === JOB AUCTIONS ===

(define-public (create-job-auction (job-id uint) (min-bid uint) (end-block uint))
  (let 
    ((auction-data {
      min-bid: min-bid,
      highest-bidder: none,
      end-block: end-block
    }))
    (asserts! (> end-block (get-block-height)) (err ERR-AUCTION-ENDED))
    (asserts! (> min-bid u0) (err ERR-BID-TOO-LOW))
    (asserts! (is-none (map-get? job-auctions job-id)) (err ERR-UNAUTHORIZED))
    (map-insert job-auctions job-id auction-data)
    (ok true)))

(define-public (place-bid (job-id uint) (bid-amount uint))
  (let 
    ((auction (unwrap! (map-get? job-auctions job-id) (err ERR-AUCTION-NOT-FOUND)))
     (end-block-val (get end-block auction))
     (min-bid-val (get min-bid auction)))
    (asserts! (<= (get-block-height) end-block-val) (err ERR-AUCTION-ENDED))
    (asserts! (>= bid-amount min-bid-val) (err ERR-BID-TOO-LOW))
    (ok (map-set job-auctions job-id
      {
        min-bid: bid-amount,
        highest-bidder: (some tx-sender),
        end-block: end-block-val
      }))))

;; === READ-ONLY ===

(define-read-only (get-job (id uint)) (ok (map-get? jobs id)))
(define-read-only (get-project (id uint)) (ok (map-get? projects id)))
(define-read-only (get-reputation (user principal)) (ok (default-to 0 (map-get? reputation user))))
(define-read-only (get-auction (id uint)) (ok (map-get? job-auctions id)))
(define-read-only (is-subscribed (user principal)) (ok (default-to false (map-get? subscribers user))))
