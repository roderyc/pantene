;;;; Conditions

;;; Conditions provide a mechanism for distinguishing different types of
;;; exceptional situations and for specifying where control should be
;;; transferred should a given situation arise. When an exception occurs, the
;;; signaler provides a condition to a condition handler which can decide how
;;; to procede with the computation using the information within the condition.
;;; Technically, arbitrary objects could be use to communicate information
;;; to handlers, but conditions provide a standard mechanism.
;;;
;;; A condition is an object of a new disjoint type with three fields:
;;;
;;; type - a condition-type object representing the class of conditions this
;;;   condition belongs to.
;;;
;;; restarters - a list of the restarters in effect when this condition was
;;;   made.
;;;
;;; field-alist - a alist of this condition's fields. The keys usually symbols
;;;   but can be any object. Uninitialized values are set to #f.
;;;
;;; Conditions are constructed explicitly with MAKE-CONDITION & the procedures
;;; returned by CONDITION-CONSTRUCTOR or implicitly with ERROR & SIGNAL
;;;
;;; A field is allocated in the dynamic environment for a list of currently
;;; available condition handlers and the types of conditions they handle,
;;; in a particular dynamic context. This list can be obtained with the
;;; CURRENT-HANDLERS procedure; the WITH-CONDITION-HANDLER procedure pushes
;;; a handler and its list of types onto the list for a particular
;;; dynamic extent.

;;;; Condition Operations

;;; (MAKE-CONDITION <type> <restarters> <field-alist>) -> condition
;;; (CONDITION? <obj>) -> boolean
;;; (CONDITION/TYPE <condition>) -> condition-type
;;; (CONDITION/RESTARTERS <condition>) -> list
;;; (CONDITION/FIELD-ALIST <condition>) -> list
;;;   MAKE-CONDITION constructs a condition; CONDITION? is the condition
;;;   disjoint type predicate; and CONDITION/TYPE, CONDITION/RESTARTERS &
;;;   CONDITION/FIELD-ALIST access the type, restarters, & field-alist fields,
;;;   respectively, of conditions.
;;;
;;; (CONDITION/REPORT-STRING <condition>) -> string
;;; (WRITE-CONDITION-REPORT <condition> <port>)
;;;   WRITE-CONDITION-REPORT writes the report specified for the condition-type
;;;   of <condition> to <port>. CONDITION/REPORT-STRING returns a string
;;;   containing a report of <condition>. This is the same report that would be
;;;   printed to a port given to WRITE-CONDITION-REPORT.
;;;
;;; (CONDITION-CONSTRUCTOR condition-type field-names) -> procedure
;;; (CONDITION-ACCESSOR condition-type field-name) -> procedure
;;; (CONDITION-PREDICATE condition-type) -> procedure
;;; (CONDITION-SIGNALLER type field-names default-handler) -> procedure
;;;   CONDITION-CONSTRUCTOR returns a procedure that returns a condition who's
;;;   type is condition-type. The procedure takes a list of restarters and a
;;;   list of field-values that should be the same length as <field-names> and
;;;   correspond, in order to them. If the values list is shorter than
;;;   <field-names>, the unspecified fields are set to #f. If field-values is
;;;   longer than field-names the extra values are ignored. CONDITION-ACCESSOR
;;;   returns a function that takes a condition of type <condition-type> and
;;;   returns the value of <field-name>. CONDITION-PREDICATE returns a predicate
;;;   that determines if a given condition is of the type <condition-type>.
;;;   CONDITION-SIGNALER returns a function that signals a condition of type
;;;   <type>, given values for the fields in <field-names>. If the signaling of
;;;   the condition returns, <default-handler> is called on the generated condition.
;;;
;;; (WITH-CONDITION-HANDLER <types> <handler> <thunk>)
;;; (CURRENT-HANDLERS) -> list
;;;   WITH-CONDITION-HANDLER invokes <thunk> after adding <handler> as a condition
;;;   handler for the conditions specified by <types>. <Types> must be a list of
;;;   condition types; signaling a condition whose type is a specialization of
;;;   any of these types will cause the handler to be invoked. By special
;;;   extension, if <types> is the empty list then the <handler> is called for
;;;   all conditions. <Handler> is a function with one argument, the condition.
;;;   Handlers may or may not return. They can signal new conditions, or
;;;   resignal the one they were given. CURRENT-HANDLERS returns a list of the
;;;   current condition handlers in the nested order in which they were
;;;   successively pushed. It is an error to modify this list.
;;;
;;; (SIGNAL <condition>)
;;; (ERROR <reason> . <arguments>)
;;; (STANDARD-ERROR-HANDLER <condition>)
;;;   The precise operation of SIGNAL depends on the condition type of
;;;   which <condition> is an instance, and the handlers established by
;;;   with-condition-handler. If <condition> is an instance of a type that is a
;;;   specialization of any of the types associated with a condition-handler
;;;   established by with-condition-handler (checked most recent first), the
;;;   corresponding handler is invoked with <condition> as the argument. Each
;;;   applicable handler is invoked and the search for a handler continues if
;;;   the handler returns normally. If no handlers apply, (or all return
;;;   normally), signal returns an unspecified value. ERROR signals a
;;;   condition using SIGNAL, and if no handler for that condition alters the flow
;;;   of control (by invoking a restarter, for example) it calls the procedure
;;;   STANDARD-ERROR-HANDLER, with the signaled condition as <condition>, which
;;;   normally prints the condition's report-string as an error message and stops
;;;   the computation, entering an error repl. Under normal circumstances error
;;;   will not return a value (it might however, if for example a restarter was
;;;   chosen from the error repl that does so). Precisely what type of condition
;;;   is signaled depends on the first argument to ERROR. If <reason> is a
;;;   condition, then that condition is signaled and the arguments are ignored.
;;;   If reason is a  condition type, then a new instance of this type
;;;   is generated and signaled; the arguments are used to generate the values of
;;;   the fields for this condition type (they are passed as the field-alist
;;;   argument to make-condition). In the most common case, however, reason is
;;;   neither a condition nor a condition type, but rather a string or symbol.
;;;   In this case a condition of type condition-type:simple-error is created
;;;   with the message field containing the reason and the irritants field
;;;   containing the arguments.

(define-record-type <condition>
  (make-condition type restarters field-alist)
  condition?
  (type           condition/type)
  (restarters     condition/restarters)
  (field-alist    condition/field-alist))

(define current-handlers (make-parameter '()))

(define (condition/report-string condition)
  (let ((call-with-output-string
         (lambda (thunk)
           (let ((port (open-output-string)))
             (thunk port)
             (get-output-string port)))))
    (call-with-output-string
     (lambda (port)
       (write-condition-report condition port)))))

(define (write-condition-report condition port)
  (let ((reporter (condition-type/reporter (condition/type condition))))
    (reporter condition port)))

(define (condition-constructor condition-type field-names)
  (if (every (lambda (x) (member x (condition-type/field-names condition-type)))
             field-names)
      (lambda (restarters . field-values)
        (let ((field-alist (map (lambda (x) (cons x #f))
                                (condition-type/field-names condition-type))))
          (for-each (lambda (x y) (set-cdr! (assoc x field-alist) y))
                    field-names
                    field-values)
          (make-condition condition-type
                          restarters
                          field-alist)))))

(define (condition-accessor condition-type field-name)
  (lambda (condition)
    (if (equal? condition-type (condition/type condition))
        (let ((value (assoc field-name (condition/field-alist condition))))
          (if value (cdr value) value)))))

(define (condition-predicate condition-type)
  (lambda (condition)
    (and (condition? condition)
         (member condition-type
                 (condition-type/generalizations (condition/type condition))))))

(define (condition-signaller type field-names default-handler)
  (let ((constructor (condition-constructor type field-names)))
    (lambda field-values
      (let ((condition
             (apply constructor
                    (cons* (current-restarters)
                           field-values))))
        (signal condition)
        (default-handler condition)))))

(define (access-condition condition field-name)
  ((condition-accessor (condition/type condition) field-name) condition))

(define (with-condition-handler types handler thunk)
  (parameterize ((current-handlers
                  (cons (cons types handler) (current-handlers))))
                (thunk)))

(define (signal condition)
  (let* ((type-predicate (lambda (type) (member type
                                                (condition-type/generalizations
                                                 (condition/type condition)))))
         (handlers (find-tail (lambda (x) (or (null? (car x))
                                              (any type-predicate (car x))))
                              (current-handlers))))
    (do ((handler-list handlers (cdr handler-list)))
        ((null? handler-list))
      (parameterize ((current-handlers (cdr handler-list)))
                    ((cdar handler-list) condition)))))

(define (standard-error-handler condition)
  (system:error (condition/report-string condition)))

(define (error reason . arguments)
  (let ((condition (cond ((condition? reason) reason)
                         ((condition-type? reason)
                          (make-condition reason (current-restarters) arguments))
                         (else (make-condition condition-type:simple-error
                                               (current-restarters)
                                               (list (cons 'message reason)
                                                     (cons 'irritants arguments)))))))
    (signal condition)
    (standard-error-handler condition)))

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
