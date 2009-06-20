;;; Record type representing a condition object
;;; A condition object consists of a type, continuation, list of restarts, and
;;; alist of fields

;;; %make-condition : condition-type continuation restarts field-alist
;;; is a private function that constructs and returns a new condition.

;;; condition? : object
;;; returns #f if and only if object is not a condition.

;;; condition/type : condition
;;; returns the condition-type of the given condition.

;;; condition-restarters : condition
;;; returns the list of restarts for condition. These are usually the restarts
;;; in effect when the condition was created.

;;; condition/continuation : condition
;;; returns the continuation specified when condition was created. The
;;; continuation is for debugging purposes, not for continuing or restarting
;;; the computation.

;;; %condition/field-alist : condition
;;; returns an alist of condition's fields. Field names can be any object, but
;;; are usually symbols or strings. Uninitialized fields are set to #f.
(define-record-type condition
  (%make-condition condition-type continuation restarters field-alist)
  condition?
  (condition-type condition/type)
  (continuation   condition/continuation)
  (restarters     condition-restarters)
  (field-alist    %condition/field-alist))

;;; A list of the condition handlers in effect for the current dynamic extent
(define *dynamic-handlers* (make-parameter '()))

;;; Invokes thunk after adding handler as a condition handler for the conditions
;;; specified by types. types must be a list of condition types; signaling
;;; a condition whose type is a specialization of any of these types will
;;; cause the handler to be invoked. By special extension, if types is the empty
;;; list then the handler is called for all conditions.
;;; handler is a function with one argument, the condition.
;;; handler may process a signal in any way it deems appropriate, but the common
;;; patterns are:

;;; Ignore the condition.
;;;    By returning from the handler in the usual manner.

;;; Handle the condition.
;;;    By doing some processing and then invoking a restart (or, less preferably, a continuation)
;;;    that was established at some point prior to the call to signal-condition.

;;; Resignal a condition.
;;;    By doing some processing and calling signal-condition with either the same condition or a
;;;    newly created one. In order to support this, signal-condition runs handler in such a way
;;;    that a subsequent call to signal-condition sees only the handlers that were established
;;;    prior to this one.
(define (bind-condition-handler types handler thunk)
  (parameterize ((*dynamic-handlers*
                  (cons (cons types handler) (*dynamic-handlers*))))
                (thunk)))

;;; The precise operation of signal-condition depends on the condition type of
;;; which condition is an instance, the handlers established by
;;; bind-condition-handler. If the condition is an instance of a type that is a
;;; specialization of any of the types established by bind-condition-handler
;;; (checked most recent first), the corresponding handler is invoked with
;;; condition as the argument. Each applicable handler is invoked and the search
;;; for a handler continues if the handler returns normally. If no handlers apply,
;;; (or all return in a normal manner), signal-condition returns an unspecified
;;; value.
(define (signal-condition condition)
  (let* ((type-predicate (lambda (type) (member type
                                                (condition-type/generalizations
                                                 (condition/type condition)))))
         (handlers (find-tail (lambda (x) (or (null? (car x))
                                              (any type-predicate (car x))))
                              (*dynamic-handlers*))))
    (do ((handler-list handlers (cdr handler-list)))
        ((not (pair? handler-list)))
      (parameterize ((*dynamic-handlers* (cdr handler-list)))
                    ((cdar handler-list) condition)))))

;;; Prints the given condition to standard out. This should prompt the user to
;;; select a restart and enter a debugging repl, but this needs to be discussed.
(define (standard-error-handler condition)
  (system:error (condition/report-string condition)))

;;; Executes thunk with a condition handler that intercepts the signaling of any
;;; specialization of condition-type:error and immediately terminates the execution
;;; of thunk and returns from the call to ignore-errors with the signaled
;;; condition as its value. If thunk returns normally, its value is returned.
(define (ignore-errors thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler (list condition-type:error)
                             continuation
                             thunk))))

;;; error signals a condition (using signal-condition), and if no handler for that
;;; condition alters the flow of control (by invoking a restart, for example) it
;;; calls the procedure standard-error-handler, which normally prints an error
;;; message and stops the computation, entering an error repl. Under normal
;;; circumstances error will not return a value.
;;; Precisely what condition is signaled depends on the first argument to error.
;;; If reason is a condition, then that condition is signaled and the arguments
;;; are ignored. If reason is a  condition type, then a new instance of this type
;;; is generated and signaled; the arguments are used to generate the values of
;;; the fields for this condition type (they are passed as the field-alist argument
;;; to make-condition). In the most common case, however, reason is neither a
;;; condition nor a condition type, but rather a string or symbol. In this case a
;;; condition of type condition-type:simple-error is created with the message field
;;; containing the reason and the irritants field containing the arguments.
(define (error reason . arguments)
  (let ((condition (cond ((condition? reason) reason)
                         ((condition-type? reason)
                          (make-condition reason #f (current-restarters) arguments))
                         (else (make-condition condition-type:simple-error
                                               #f
                                               (current-restarters)
                                               (list (cons 'message reason)
                                                     (cons 'irritants arguments)))))))
    (signal-condition condition)
    (standard-error-handler condition)))

;;; Returns a condition object like %make-condition, but restarts can either be
;;; a list of restarts, or a condition, in which case the restarts field of the
;;; condition is used.
(define (make-condition condition-type continuation restarters field-alist)
  (letrec ((restart-argument (lambda (restarters)
                               (if (condition? restarters)
                                   (condition-restarters restarters)
                                   (list-copy restarters)))))
    (%make-condition condition-type
                     continuation
                     (restart-argument restarters)
                     field-alist)))

;;; Returns true if condition is a specialization of condition-type:error.
(define (condition/error? condition)
  (condition-type/error? (condition/type condition)))

;;; Returns a string containing a report of condition. This is the same report
;;; that would be printed to a port given to write-condition-report.
(define (condition/report-string condition)
  (let ((call-with-output-string
         (lambda (thunk)
           (let ((port (open-output-string)))
             (thunk port)
             (get-output-string port)))))
    (call-with-output-string
     (lambda (port)
       (write-condition-report condition port)))))

;;; Writes the report specified for the condition-type of condition to
;;; the given port. If condition is an error, errors signaled by the
;;; condition-type's reporter are ignored.
(define (write-condition-report condition port)
  (let ((reporter (condition-type/reporter (condition/type condition))))
    (if (condition/error? condition)
	(ignore-errors (lambda () (reporter condition port)))
	(reporter condition port))))

;;; Returns a function that returns a condition who's type is condition-type.
;;; continuation and restarts are the same specified in make-condition. field-values
;;; should be the same length as field-names and should be in the same order.
;;; If field-values is shorter than field-names, the unspecified fields are
;;; set to #f. If field-values is longer than field-names the extra values
;;; are ignored.
(define (condition-constructor condition-type field-names)
  (if (every (lambda (x) (member x (condition-type/field-names condition-type)))
             field-names)
      (lambda (continuation restarters . field-values)
        (let ((field-alist (map (lambda (x) (cons x #f))
                                (condition-type/field-names condition-type))))
          (for-each (lambda (x y) (set-cdr! (assoc x field-alist) y))
                    field-names
                    field-values)
          (make-condition condition-type
                          continuation
                          restarters
                          field-alist)))))

;;; Returns a function that takes a condition of type condition-type and returns
;;; the value of the specified field-name.
;;; (not implemented)
;;; If condition is not of type condition-type, a wrong-type-argument condition
;;; is signaled. If the condition-type does not have a field field-name, *
(define (condition-accessor condition-type field-name)
  (lambda (condition)
    (if (equal? condition-type (condition/type condition))
        (let ((value (assoc field-name (%condition/field-alist condition))))
          (if value (cdr value) value)))))

;;; Returns a predicate that determines if a given condition is
;;; of the type condition-type.
(define (condition-predicate condition-type)
  (lambda (condition)
    (and (condition? condition)
         (member condition-type
                 (condition-type/generalizations (condition/type condition))))))

;;; Returns a function that signals a condition of type type, given values for
;;; the fields in field-names. If signal-condition returns, default-handler
;;; is called on the generated condition.
(define (condition-signaller type field-names default-handler)
  (let ((constructor (condition-constructor type field-names)))
    (lambda field-values
      (call-with-current-continuation
       (lambda (continuation)
	 (let ((condition
		(apply constructor
		       (cons* continuation
                              (current-restarters)
                              field-values))))
	   (signal-condition condition)
	   (default-handler condition)))))))

;;; Returns the value of field-name in condition. If condition does not have
;;; a field field-name *
(define (access-condition condition field-name)
  ((condition-accessor (condition/type condition) field-name) condition))


(define error:wrong-type-argument
  (condition-signaller condition-type:wrong-type-argument
                       '(datum type operator)
                       standard-error-handler))

(define error:bad-range-argument
  (condition-signaller condition-type:bad-range-argument
                       '(datum operator)
                       standard-error-handler))

(define error:datum-out-of-range
  (condition-signaller condition-type:datum-out-of-range
                       '(datum)
                       standard-error-handler))

(define error:no-such-restarter
  (condition-signaller condition-type:no-such-restarter
                       '(name)
                       standard-error-handler))
