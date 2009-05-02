;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure
    srfi-89
    srfi-89/interface
  (open scheme srfi-1 let-opt)
  (for-syntax (open scheme let-opt (subset signals (syntax-error)) srfi-1))
  (files srfi-89))

(define-structure
    srfi-39
    srfi-39/interface
  (open scheme)
  (files srfi-39))

(define-structure
    pantene:condition-type-definition
    pantene:condition-type-definition/interface
  (open scheme srfi-9)
  (files condition-type))

(define-structure
    pantene:condition-definition
    pantene:condition-definition/interface
  (open scheme srfi-1 srfi-9)
  (files condition))

(define-structure
    pantene:restarts
    pantene:restarts/interface
  (open scheme srfi-1 srfi-9 srfi-39 srfi-89 signals)
  (files restart))

(define-structure pantene pantene/interface
  (open scheme
        pantene:condition-type-definition
        pantene:condition-definition
        pantene:restarts))
