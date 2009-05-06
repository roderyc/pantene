(define-record-type condition-type
  (%make-condition-type name generalizations field-names reporter)
  condition-type?
  (name             condition-type/name)
  (generalizations  condition-type/generalizations
                    condition-type/set-generalizations)
  (field-names      condition-type/field-names)
  (reporter         condition-type/reporter))

(define (make-condition-type name generalization field-names reporter)
  (let* ((condition-type (%make-condition-type name
                                               #f
                                               field-names
                                               reporter))
         (generalizations (cons condition-type
                                (if generalization
                                    (condition-type/generalizations generalization)
                                    '()))))
    (condition-type/set-generalizations condition-type generalizations)
    condition-type))

(define (condition-type/error? condition)
  (member condition-type:error
          (condition-type/generalizations condition)))
