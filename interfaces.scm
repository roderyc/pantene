;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-interface srfi-89/interface
  (export define*))

(define-interface srfi-39/interface
  (export make-parameter parameterize))

(define-interface pantene:condition-type-definition/interface
  (export make-condition-type
          condition-type/error?
          condition-type/field-names
          condition-type/generalizations
          condition-type?))

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
          access-condition))

(define-interface pantene:restarts/interface
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
          restart/interactor))

(define-interface pantene/interface
  (compound-interface
   pantene:condition-type-definition/interface
   (export)))

