;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure srfi-89 srfi-89/interface
  (open scheme srfi-1 let-opt)
  (for-syntax (open scheme let-opt (subset signals (syntax-error)) srfi-1))
  (files srfi-89))

(define-structure srfi-39 srfi-39/interface
  (open scheme)
  (files srfi-39))

(define-structures
    ((pantene:condition-definition pantene:condition-definition/interface)
     (pantene:condition-type       pantene:condition-type/interface)
     (pantene:restarts             pantene:restarts/interface))
  (open scheme
        (modify signals (rename (error system:error)))
        srfi-1 srfi-6 srfi-9 srfi-13 srfi-14 srfi-39 srfi-89 signals)
  (files condition-type
         taxonomy
         condition
         restart))

(define-structure pantene pantene/interface
  (open scheme
        pantene:condition-type
        pantene:condition-definition
        pantene:restarts))
