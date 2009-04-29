(define-record-type condition
  (%make-condition condition-type continuation restarts field-alist)
  condition?
  (type          condition/type)
  (restarts      condition/restarts)
  (continuation  condition/continuation)
  (field-plist   %condition/field-alist))

(define (make-condition condition-type continuation restarts field-plist)
  (letrec ((restart-argument (lambda (restarts)
                               (if (condition? restarts)
                                   (condition/restarts restarts)
                                   (list-copy restarts))))
           (plist->alist (lambda (plist)
                           (if (null? plist)
                               '()
                               (cons (cons (car plist)
                                           (cadr plist))
                                     (plist->alist (cddr plist)))))))
    (%make-condition condition-type
                     continuation
                     (restart-argument restarts)
                     (plist->alist field-plist))))

(define (%restarts-argument restarts)
  (if (condition? restarts)
      (condition/restarts restarts)
      (list-copy restarts)))

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

(define (condition-constructor condition-type field-names)
  (letrec ((interlace (lambda (first second)
                        (if (or (null? first) (null? second))
                            '()
                            (append (list (car first)
                                          (car second))
                                    (interlace (cdr first)
                                               (cdr second)))))))
  (lambda (continuation restarts . field-values)
    (make-condition condition-type
                    continuation
                    restarts
                    (interlace field-names field-values))))

(define (condition-accessor condition-type field-name)
  (lambda (condition)
    (if (equal? condition-type (condition/type condition))
        (or (cdr (assoc field-name (%condition/field-alist condition)))
            #f)
        #f)))

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