;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure let-opt-expanders let-opt-expanders/interface
  (open scheme (subset signals (error warn)) srfi-8)
  (files let-opt-expanders))

(define-structure let-opt let-opt/interface
  (open scheme (subset signals (error warn)) receiving)
  (for-syntax (open scheme let-opt-expanders))
  (files let-opt))

(define-structure srfi-89 srfi-89/interface
  (open scheme let-opt)
  (for-syntax (open scheme let-opt
                    (subset signals (syntax-error))
                    (subset srfi-1 (every take-while drop-while))))
  (files srfi-89))

(define-structures
    ((pantene:condition-definition pantene:condition-definition/interface)
     (pantene:condition-type       pantene:condition-type/interface)
     (pantene:restarter           pantene:restarter/interface))
  (open scheme
        (modify signals  (expose system:error) (rename (error system:error)))
        (subset srfi-1 (cons* every any find find-tail))
        (subset srfi-13 (string-null? string-prefix-ci?))
        srfi-6 srfi-9 srfi-14 srfi-39 srfi-89)
  (files condition-type
         taxonomy
         condition
         restarter))

(define-structure pantene pantene/interface
  (open scheme
        pantene:condition-type
        pantene:condition-definition
        pantene:restarter))

(define-structure pantene:condition-tests (export)
  (open scheme pantene srfi-78)
  (files (tests test-conditions)))

(define-structure pantene:condition-type-tests (export)
  (open scheme pantene srfi-78)
  (files (tests test-condition-types)))

(define-structure pantene:restarter-tests (export)
  (open scheme pantene srfi-78)
  (files (tests test-restarters)))

(define-structure pantene:tests (export)
  (open pantene:condition-tests
        pantene:condition-type-tests
        pantene:restarter-tests))
