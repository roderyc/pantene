(define-record-type condition-type
  (make-condition-type name generalization field-names reporter)
  condition-type?
  (name            condition-type/name)
  (generalization  condition-type/generalizations)
  (field-names     condition-type/field-names)
  (reporter        condition-type/reporter))

(define (condition-type/error? condition)
  (eq? condition-type:error
       (condition-type/generalizations condition)))
