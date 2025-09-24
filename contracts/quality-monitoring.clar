;; Healthcare Quality Assurance Contract
;; Quality monitoring system with performance metrics, compliance tracking, improvement planning, and outcome measurement

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-score (err u103))
(define-constant err-invalid-status (err u104))

;; Data Variables
(define-data-var next-provider-id uint u1)
(define-data-var next-metric-id uint u1)
(define-data-var next-assessment-id uint u1)
(define-data-var next-improvement-plan-id uint u1)

;; Data Maps
(define-map healthcare-providers
  { provider-id: uint }
  {
    name: (string-ascii 150),
    facility-type: (string-ascii 50),
    accreditation-level: (string-ascii 30),
    registration-date: uint,
    status: (string-ascii 20)
  }
)

(define-map quality-metrics
  { metric-id: uint }
  {
    metric-name: (string-ascii 100),
    category: (string-ascii 50),
    target-score: uint,
    measurement-unit: (string-ascii 30),
    frequency: (string-ascii 20),
    active: bool
  }
)

(define-map performance-assessments
  { assessment-id: uint }
  {
    provider-id: uint,
    metric-id: uint,
    assessment-date: uint,
    actual-score: uint,
    target-score: uint,
    compliance-status: (string-ascii 20),
    assessor: (string-ascii 100),
    notes: (string-ascii 300)
  }
)

(define-map improvement-plans
  { plan-id: uint }
  {
    provider-id: uint,
    plan-title: (string-ascii 150),
    target-metrics: (string-ascii 200),
    start-date: uint,
    target-completion: uint,
    status: (string-ascii 20),
    priority-level: (string-ascii 10),
    assigned-lead: (string-ascii 100)
  }
)

(define-map outcome-measurements
  { provider-id: uint, metric-id: uint, period: uint }
  {
    baseline-score: uint,
    current-score: uint,
    improvement-percentage: uint,
    trend-direction: (string-ascii 15),
    last-updated: uint,
    milestone-achieved: bool
  }
)

;; Public Functions

;; Register healthcare provider
(define-public (register-provider (name (string-ascii 150)) (facility-type (string-ascii 50)) (accreditation-level (string-ascii 30)))
  (let
    (
      (provider-id (var-get next-provider-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? healthcare-providers {provider-id: provider-id})) err-already-exists)
    
    (map-set healthcare-providers
      {provider-id: provider-id}
      {
        name: name,
        facility-type: facility-type,
        accreditation-level: accreditation-level,
        registration-date: stacks-block-height,
        status: "active"
      }
    )
    
    (var-set next-provider-id (+ provider-id u1))
    (ok provider-id)
  )
)

;; Add quality metric
(define-public (add-quality-metric (metric-name (string-ascii 100)) (category (string-ascii 50)) (target-score uint) (measurement-unit (string-ascii 30)) (frequency (string-ascii 20)))
  (let
    (
      (metric-id (var-get next-metric-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= target-score u100) err-invalid-score)
    
    (map-set quality-metrics
      {metric-id: metric-id}
      {
        metric-name: metric-name,
        category: category,
        target-score: target-score,
        measurement-unit: measurement-unit,
        frequency: frequency,
        active: true
      }
    )
    
    (var-set next-metric-id (+ metric-id u1))
    (ok metric-id)
  )
)

;; Conduct performance assessment
(define-public (conduct-assessment (provider-id uint) (metric-id uint) (actual-score uint) (assessor (string-ascii 100)) (notes (string-ascii 300)))
  (let
    (
      (assessment-id (var-get next-assessment-id))
      (provider (unwrap! (map-get? healthcare-providers {provider-id: provider-id}) err-not-found))
      (metric (unwrap! (map-get? quality-metrics {metric-id: metric-id}) err-not-found))
      (target-score (get target-score metric))
      (compliance-status (if (>= actual-score target-score) "compliant" "non-compliant"))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= actual-score u100) err-invalid-score)
    (asserts! (get active metric) err-invalid-status)
    
    (map-set performance-assessments
      {assessment-id: assessment-id}
      {
        provider-id: provider-id,
        metric-id: metric-id,
        assessment-date: stacks-block-height,
        actual-score: actual-score,
        target-score: target-score,
        compliance-status: compliance-status,
        assessor: assessor,
        notes: notes
      }
    )
    
    (var-set next-assessment-id (+ assessment-id u1))
    (ok assessment-id)
  )
)

;; Create improvement plan
(define-public (create-improvement-plan (provider-id uint) (plan-title (string-ascii 150)) (target-metrics (string-ascii 200)) (target-completion uint) (priority-level (string-ascii 10)) (assigned-lead (string-ascii 100)))
  (let
    (
      (plan-id (var-get next-improvement-plan-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? healthcare-providers {provider-id: provider-id})) err-not-found)
    
    (map-set improvement-plans
      {plan-id: plan-id}
      {
        provider-id: provider-id,
        plan-title: plan-title,
        target-metrics: target-metrics,
        start-date: stacks-block-height,
        target-completion: target-completion,
        status: "active",
        priority-level: priority-level,
        assigned-lead: assigned-lead
      }
    )
    
    (var-set next-improvement-plan-id (+ plan-id u1))
    (ok plan-id)
  )
)

;; Update outcome measurement
(define-public (update-outcome-measurement (provider-id uint) (metric-id uint) (period uint) (baseline-score uint) (current-score uint))
  (let
    (
      (improvement-percentage (if (> baseline-score u0) (/ (* (- current-score baseline-score) u100) baseline-score) u0))
      (trend-direction (if (> current-score baseline-score) "improving" (if (< current-score baseline-score) "declining" "stable")))
      (milestone-achieved (>= current-score (get target-score (unwrap! (map-get? quality-metrics {metric-id: metric-id}) err-not-found))))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? healthcare-providers {provider-id: provider-id})) err-not-found)
    (asserts! (is-some (map-get? quality-metrics {metric-id: metric-id})) err-not-found)
    (asserts! (<= baseline-score u100) err-invalid-score)
    (asserts! (<= current-score u100) err-invalid-score)
    
    (map-set outcome-measurements
      {provider-id: provider-id, metric-id: metric-id, period: period}
      {
        baseline-score: baseline-score,
        current-score: current-score,
        improvement-percentage: improvement-percentage,
        trend-direction: trend-direction,
        last-updated: stacks-block-height,
        milestone-achieved: milestone-achieved
      }
    )
    
    (ok true)
  )
)

;; Update improvement plan status
(define-public (update-plan-status (plan-id uint) (new-status (string-ascii 20)))
  (let
    (
      (plan (unwrap! (map-get? improvement-plans {plan-id: plan-id}) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set improvement-plans
      {plan-id: plan-id}
      (merge plan {status: new-status})
    )
    
    (ok true)
  )
)

;; Update provider status
(define-public (update-provider-status (provider-id uint) (new-status (string-ascii 20)))
  (let
    (
      (provider (unwrap! (map-get? healthcare-providers {provider-id: provider-id}) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set healthcare-providers
      {provider-id: provider-id}
      (merge provider {status: new-status})
    )
    
    (ok true)
  )
)

;; Read-only Functions

;; Get healthcare provider information
(define-read-only (get-provider (provider-id uint))
  (map-get? healthcare-providers {provider-id: provider-id})
)

;; Get quality metric information
(define-read-only (get-quality-metric (metric-id uint))
  (map-get? quality-metrics {metric-id: metric-id})
)

;; Get performance assessment
(define-read-only (get-assessment (assessment-id uint))
  (map-get? performance-assessments {assessment-id: assessment-id})
)

;; Get improvement plan
(define-read-only (get-improvement-plan (plan-id uint))
  (map-get? improvement-plans {plan-id: plan-id})
)

;; Get outcome measurement
(define-read-only (get-outcome-measurement (provider-id uint) (metric-id uint) (period uint))
  (map-get? outcome-measurements {provider-id: provider-id, metric-id: metric-id, period: period})
)

;; Get next available IDs
(define-read-only (get-next-provider-id)
  (var-get next-provider-id)
)

(define-read-only (get-next-metric-id)
  (var-get next-metric-id)
)

(define-read-only (get-next-assessment-id)
  (var-get next-assessment-id)
)

(define-read-only (get-next-improvement-plan-id)
  (var-get next-improvement-plan-id)
)


;; title: quality-monitoring
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

