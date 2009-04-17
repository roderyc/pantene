;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure
    pantene:condition-type-definition
    pantene:condition-type-definition/interface
  (open scheme srfi-9)
  (files condition-type))

(define-interface pantene/interface
  (compound-interface
   pantene:condition-type-definition/interface
   (export)))

(define-structure pantene pantene/interface
  (open scheme
        pantene:condition-type-definition))