;Record type representing a condition of some sort
;The condition abstraction consists of a type, continuation, list of restarts,
;and alist of fields
;
;%make-condition : condition-type continuation restarts field-alist
;is a private function that constructs and returns a new condition.
;
;condition? : object
;returns #f if and only if object is not a condition.
;
;condition/type : condition
;returns the condition-type of the given condition.
;
;condition/restarts : condition
;returns the list of restarts for condition.
;
;condition/continuation : condition
;returns the continuation specified when condition was created. The
;continuation is for debugging purposes, not for continuing or restarting
;the computation.
;
;%condition/field-alist : condition
;returns an alist of condition's fields. Field names can be any object, but
;are usually symbols or strings. Uninitialized fields are set to #f.
(define-record-type condition
  (%make-condition condition-type continuation restarts field-alist)
  condition?
  (condition-type condition/type)
  (continuation   condition/continuation)
  (restarts       condition/restarts)
  (field-alist    %condition/field-alist))

;Returns a condition object like %make-condition, but restarts can either be
;a list of restarts, or a condition, in which case the restarts field of the
;condition is use.d
(define (make-condition condition-type continuation restarts field-alist)
  (letrec ((restart-argument (lambda (restarts)
                               (if (condition? restarts)
                                   (condition/restarts restarts)
                                   (list-copy restarts)))))
    (%make-condition condition-type
                     continuation
                     (restart-argument restarts)
                     field-alist)))

;Returns true  if condition is an error of some sort (it's type descends from
;condition-type:error).
(define (condition/error? condition)
  (condition-type/error? (condition/type condition)))

;Returns a string containing a report of condition. This is the same report
;that would be printed to a port given to write-condition-report.
(define (condition/report-string condition)
  (call-with-output-string
   (lambda (port)
     (write-condition-report condition port))))

;Writes the report specified for he condition-type of condition to
;the given port.
;If condition is an error, errors signaled by the condition-type's reporter
;are ignored.
(define (write-condition-report condition port)
  (let ((reporter (condition-type/reporter (condition/type condition))))
    (if (condition/error? condition)
	(ignore-errors (lambda () (reporter condition port)))
	(reporter condition port))))

;Returns a function that returns a condition who's type is condition-type.
;continuation and restarts are the same specified in make-condition. field-values
;should be the same length as field-names and should be in the same order.
;If field-values is shorter than field-names, the unspecified fields are
;set to #f. If field-values is longer than field-names the extra values
;are ignored.
(define (condition-constructor condition-type . field-names)
  (if (every (lambda (x) (member x (condition-type/field-names condition-type)))
             field-names)
      (lambda (continuation restarts . field-values)
        (let ((field-alist (map (lambda (x) (cons x #f))
                                (condition-type/field-names condition-type))))
          (for-each (lambda (x y) (set-cdr! (assoc x field-alist) y))
                    field-names
                    field-values)
          (make-condition condition-type
                          continuation
                          restarts
                          field-alist)))))

;Returns a function that takes a condition of type condition-type and returns
;the value of the specified field-name.
;(not implemented)
;If condition is not of type condition-type, a wrong-type-argument condition
;is signalled. If the condition-type does not have a field field-name, *
(define (condition-accessor condition-type field-name)
  (lambda (condition)
    (if (equal? condition-type (condition/type condition))
        (let ((value (assoc field-name (%condition/field-alist condition))))
          (if value (cdr value) value)))))

;Returns a predicate that determines if a given condition is
;of the type condition-type.
(define (condition-predicate condition-type)
  (lambda (condition)
    (and (condition? condition)
         (member condition-type
                 (condition-type/generalizations (condition/type condition))))))

;Returns a function that signals a condition of type type, given values for
;the fields in field-names. If signal-condition returns, default-handler
;is called on the generated condition.
(define (condition-signaller type field-names default-handler)
  (let ((constructor (apply condition-constructor `(,type ,@field-names))))
    (lambda (field-values)
      (call-with-current-continuation
       (lambda (continuation)
	 (let ((condition
		(apply constructor
		       `(,continuation
                         ,(bound-restarts)
                         ,@field-values))))
	   (signal-condition condition)
	   (default-handler condition)))))))

;Returns the value of field-name in condition. If condition does not have
;a field field-name *
(define (access-condition condition field-name)
  ((condition-accessor (condition/type condition) field-name) condition))
