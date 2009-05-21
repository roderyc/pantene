(define-record-type condition-type
  (%make-condition-type name generalizations field-names reporter)
  condition-type?
  (name             condition-type/name)
  (generalizations  condition-type/generalizations
                    condition-type/set-generalizations!)
  (field-names      condition-type/field-names)
  (reporter         condition-type/reporter))

(define (make-condition-type name generalization field-names reporter)
  (let* ((reporter (cond ((string? reporter)
                          (lambda (condition port)
                            (display reporter port)))
                         ((procedure? reporter) reporter)
                         ((not reporter)
                          (if generalization
                              (condition-type/reporter generalization)
                              (lambda (condition port)
                                (display "undocumented condition of type "
                                         port)
                                (display (condition/type condition) port))))
                         (else
                          (error:wrong-type-argument reporter
                                                     "condition-type reporter"
                                                     'make-condition-type))))
         (condition-type (%make-condition-type name #f field-names reporter))
         (generalizations (cons condition-type
                                (if generalization
                                    (condition-type/generalizations generalization)
                                    '()))))
    (condition-type/set-generalizations! condition-type generalizations)
    condition-type))

(define (condition-type/error? condition)
  (member condition-type:error
          (condition-type/generalizations condition)))
