;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-interface pantene:condition-type-definition/interface
  (export make-condition-type
          condition-type/error?
          condition-type/field-names
          condition-type/generalizations
          condition-type?))
