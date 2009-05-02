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
;returns an alist of condition's fields.
(define-record-type condition
  (%make-condition condition-type continuation restarts field-alist)
  condition?
  (condition-type condition/type)
  (continuation   condition/continuation)
  (restarts       condition/restarts)
  (field-alist    %condition/field-alist))

(define (make-condition condition-type continuation restarts field-alist)
  (letrec ((restart-argument (lambda (restarts)
                               (if (condition? restarts)
                                   (condition/restarts restarts)
                                   (list-copy restarts)))))
    (%make-condition condition-type
                     continuation
                     (restart-argument restarts)
                     field-alist)))

(define (condition/error? condition)
  (condition-type/error? (condition/type condition)))

(define (condition/report-string condition)
  (call-with-output-string
   (lambda (port)
     (write-condition-report condition port))))

(define (write-condition-report condition port)
  (let ((reporter (condition-type/reporter (condition/type condition))))
    (if (condition/error? condition)
	(ignore-errors (lambda () (reporter condition port)))
	(reporter condition port))))

(define (condition-constructor condition-type . field-names)
  (lambda (continuation restarts . field-values)
    (make-condition condition-type
                    continuation
                    restarts
                    (map (lambda (x y) (cons x y))
                         field-names
                         (append field-values (circular-list #f))))))

(define (condition-accessor condition-type field-name)
  (lambda (condition)
    (if (equal? condition-type (condition/type condition))
        (let ((value (assoc field-name (%condition/field-alist condition))))
          (if value (cdr value) value)))))

(define (condition-predicate condition-type)
  (lambda (condition)
    (and (condition? condition)
         (member condition-type
                 (condition-type/generalizations (condition/type condition))))))

(define (condition-signaller type field-names default-handler)
  (let ((constructor (condition-constructor type field-names)))
    (lambda (field-values)
      (call-with-current-continuation
       (lambda (continuation)
	 (let ((condition
		(apply constructor
		       (list continuation
			     (bound-restarts)
                             field-values))))
	   (signal-condition condition)
	   (default-handler condition)))))))

(define (access-condition condition field-name)
  ((condition-accessor (condition/type condition) field-name) condition))
