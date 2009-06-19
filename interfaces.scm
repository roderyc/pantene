;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-interface let-opt-expanders/interface
  (export expand-let-optionals
          expand-let-optionals*))

(define-interface let-opt/interface
  (export (let-optionals  :syntax)
          (let-optionals* :syntax)
          (:optional      :syntax)))

(define-interface srfi-8/interface
  (export receive))

(define-interface srfi-89/interface
  (export define*))

(define-interface srfi-78/interface
  (export (check :syntax)
          (check-ec :syntax)
          check-report
          check-set-mode!
          check-reset!
          check-passed?))

(define-interface srfi-39/interface
  (export make-parameter parameterize))

(define-interface pantene:condition-type/interface
  (export make-condition-type
          condition-type/name
          condition-type/error?
          condition-type/field-names
          condition-type/generalizations
          condition-type/reporter
          condition-type?
          condition-type:serious-condition
          condition-type:error
          condition-type:simple-error
          condition-type:illegal-datum
          condition-type:datum-out-of-range
          condition-type:wrong-type-datum
          condition-type:wrong-type-argument
          condition-type:bad-range-argument
          condition-type:control-error
          condition-type:no-such-restart
          condition-type:file-error
          condition-type:primitive-procedure-error))

(define-interface pantene:condition-definition/interface
  (export make-condition
          condition?
          condition/type
          condition/error?
          condition/restarts
          condition/continuation
          condition/report-string
          condition-constructor
          condition-accessor
          condition-predicate
          condition-signaller
          access-condition
          bind-condition-handler
          standard-error-handler
          signal-condition
          ignore-errors
          error
          error:wrong-type-argument
          error:bad-range-argument
          error:datum-out-of-range
          error:no-such-restart))

(define-interface pantene:restarter/interface
  (export with-restart
          with-simple-restart
          bound-restarts
          invoke-restart
          invoke-restart-interactively
          find-restart
          restart?
          restart/name
          restart/description
          restart/effector
          restart/interactor
          abort
          continue
          muffle-warning
          retry
          store-value
          use-value
          (restart-case :syntax)))

(define-interface pantene/interface
  (compound-interface
   pantene:condition-type/interface
   pantene:condition-definition/interface
   pantene:restarter/interface))
