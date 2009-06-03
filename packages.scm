;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure srfi-8 srfi-8/interface
  (open scheme)
  (files srfi-8))

(define-structure let-opt-expanders let-opt-expanders/interface
  (open scheme
	signals
	srfi-8)
  (files let-opt-expanders))

(define-structure let-opt let-opt/interface
  (open scheme signals)
  (for-syntax (open scheme let-opt-expanders))
  (files let-opt))

(define-structure srfi-89 srfi-89/interface
  (open scheme srfi-1 let-opt)
  (for-syntax (open scheme let-opt (subset signals (syntax-error)) srfi-1))
  (files srfi-89))

(define-structure srfi-78 srfi-78/interface
  (open scheme srfi-23 srfi-42 pp)
  (files srfi-78)
  (begin (define check:write p)))

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

(define-structure pantene:condition-tests (export)
  (open scheme pantene srfi-78)
  (files (tests test-conditions)))

(define-structure pantene:condition-type-tests (export)
  (open scheme pantene srfi-78)
  (files (tests test-condition-types)))

(define-structure pantene:restart-tests (export)
  (open scheme pantene srfi-1 srfi-78)
  (files (tests test-restarts)))

(define-structure pantene:tests (export)
  (open pantene:condition-tests
        pantene:condition-type-tests
        pantene:restart-tests))
