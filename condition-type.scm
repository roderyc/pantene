(define-record-type condition-type
  (make-condition-type name generalization field-names reporter)
  condition-type?
  (name             condition-type/name)
  (generalizations  condition-type/generalizations)
  (field-names      condition-type/field-names)
  (reporter         condition-type/reporter))

(define (condition-type/error? condition)
  (member condition-type:error
          (condition-type/generalizations condition)))
