;;; Record type representing a restart object.
;;; A restart object consists of a name, description, effector and interactor.

;;; %make-restart : name description effector interactor
;;; is a private function that constructs and returns a new restart object.

;;; restart? : object
;;; returns #f if and only if object is not a restart.

;;; restart/name : restart
;;; returns restart's name. A name can be any object, but is usually a
;;; symbol or #f.

;;; restart/description : restart
;;; returns restart's description.

;;; restart/effector : restart
;;; returns the effector encapsulated in restart. An effector can be used to
;;; continue, abort, or restart the computation it's restart object was created in.

;;; restart/interactor : restart
;;; returns the interactor encapsulated in the given restart. An interactor must
;;; be either a function that takes no arguments and returns the number of
;;; values restart's effector expects to receive, or #f if the restart is not
;;; meant to be invoked interactively.
(define-record-type restart
  (%make-restart name description effector interactor)
  restart?
  (name restart/name)
  (description restart/description)
  (effector restart/effector)
  (interactor restart/interactor))

;;; A list of the restarts in effect for the current dynamic extent
(define *current-restarts* (make-parameter '()))

;;; Returns a list of the restarts currently in effect
(define (bound-restarts) (list-copy (*current-restarts*)))

;;; Calls thunk with a restart encapsulating the given name, description,
;;; and interactor in effect during it's execution.
(define (with-restart name description effector interactor thunk)
  (parameterize ((*current-restarts*
                  (cons (%make-restart name description effector interactor)
                        (*current-restarts*))))
    (thunk)))

;;; Calls thunk with a restart encapsulating the given name and description
;;; and with an effector that continues the current computation, returning
;;; no values. Its interactor does not collect arguments and returns no values.
(define (with-simple-restart name description thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restart name description continuation values thunk))))

;;; Calls restart's effector using args as arguments.
(define (invoke-restart restart . args)
  (if (restart? restart)
      (apply (restart/effector restart) args)
      (error "invalid restart argument"
             `(invoke-restart ,restart ,@args))))

;;; First calls the interactor encapsulated in restart to interactively gather
;;; the arguments needed for restart's effector. It then calls the effector,
;;; passing these arguments to it.
(define (invoke-restart-interactively restart)
  (if (and (restart? restart)
           (restart/interactor restart))
      (call-with-values (restart/interactor restart)
        (restart/effector restart))
      (error "invalid restart argument"
             `(invoke-restart-interactively ,restart))))

;;; Returns the first restart object named name in restarts. restarts can be
;;; either a list of restart objects, or a condition, in which case the list of
;;; restarts given by condition/restarts is searched. If restarts is not supplied,
;;; the list return by (bound-restarts) is searched. Returns false if there's
;;; no restart with that name.
;;; When used in a condition handler, find-restart is usually passed the name of a
;;; particular restart and the condition object that has been signalled. In this
;;; way the handler finds only restarts that were available when the condition was
;;; created (usually the same as when it was signalled).
(define* (find-restart name (restarts #f))
  (let ((restart-list
         (cond ((not restarts) (bound-restarts))
               ((condition? restarts) (condition/restarts restarts))
               ((list? restarts) restarts)
               (else '()))))
    (find (lambda (x) (equal? (restart/name x) name)) restart-list)))

;;; Pantene supports six standard protocols for restarting from a condition,
;;; each encapsulated using a named restart (for use by condition-signalling code)
;;; and a simple procedure (for use by condition-handling code). Unless otherwise
;;; specified, if one of these procedures is unable to find its corresponding
;;; restart, it returns immediately with an unspecified value.
;;; Each of these procedures accepts an optional argument restarts; the same
;;; described by find-restart.

;;; Abort the computation, using the restart named abort. The corresponding
;;; effector takes no arguments and abandons the current line of computation.
;;; If there is no restart named abort, this procedure signals an error of type
;;; condition-type:no-such-restart.
(define* (abort (restarts #f))
  (let ((restart (find-restart 'abort restarts)))
    (if restart
        (invoke-restart restart)
        (error:no-such-restart 'abort))))

;;; Continue the current computation, using the restart named continue. The
;;; corresponding effector takes no arguments and continues the computation
;;; beyond the point at which the condition was signalled.
(define* (continue (restarts #f))
  (let ((restart (find-restart 'continue restarts)))
    (if restart
        (invoke-restart restart))))

;;; Continue the current computation, using the restart named muffle-warning.
;;; The corresponding effector takes no arguments and continues the computation
;;; beyond the point at which any warning message resulting from the condition
;;; would be presented to the user. The procedure warn establishes a muffle-warning
;;; restart for this purpose.
;;; If there is no restart named muffle-warning, this procedure signals an error
;;; of type condition-type:no-such-restart.
(define* (muffle-warning (restarts #f))
  (let ((restart (find-restart 'muffle-warning restarts)))
    (if restart
        (invoke-restart restart)
        (error:no-such-restart 'muffle-warning))))

;;; Retry the current computation, using the restart named retry. The
;;; corresponding effector takes no arguments and simply retries the same
;;; computation that triggered the condition. The condition may reoccur, of
;;; course, if the root cause has not been eliminated. The code that signals a
;;; "file does not exist" error can be expected to supply a retry restart. The
;;; restart would be invoked after first creating the missing file, since the
;;; computation is then likely to succeed if it is simply retried.
(define* (retry (restarts #f))
  (let ((restart (find-restart 'retry restarts)))
    (if restart
        (invoke-restart restart))))

;;; Retry the current computation, using the restart named store-value, after
;;; first storing new-value. The corresponding effector takes one argument,
;;; new-value, and stores it away in a restart-dependent location, then retries
;;; the same computation that triggered the condition. The condition may reoccur,
;;; of course, if the root cause has not been eliminated. The code that signals
;;; an "unassigned variable" error can be expected to supply a store-value restart;
;;; this would store the value in the variable and continue the computation.
(define* (store-value value (restarts #f))
  (let ((restart (find-restart 'store-value restarts)))
    (if restart
        (invoke-restart restart value))))

;;; Retry the current computation, using the restart named use-value, but
;;; substituting new-value for a value that previously caused a failure. The
;;; corresponding effector takes one argument, new-value, and retries the
;;; same computation that triggered the condition with the new value substituted
;;; for the failing value. The condition may reoccur, of course, if the new value
;;; also induces the condition.
;;; The code that signals an "unassigned variable" error can be expected to
;;; supply a use-value restart; this would simply continue the computation with
;;; new-value instead of the value of the variable. Contrast this with the retry
;;; and store-value restarts. If the retry restart is used it will fail because
;;; the variable still has no value. The store-value restart could be used, but
;;; it would alter the value of the variable, so that future references to the
;;; variable would not be detected.
(define* (use-value value (restarts #f))
  (let ((restart (find-restart 'use-value restarts)))
    (if restart
        (invoke-restart restart value))))

;;;; Restart Syntax
;;; (restart-case <expression> <clause> ...)
;;; Evaluates <expression> in a dynamic environment where restarts represented
;;; by each <clause> are in effect. If none of the restarts are invoked by
;;; <expression>, any and all values it returns are returned by restart-case.
;;; If a restart is invoked, the effector of that restart is evaluated and
;;; any values it returns are returned by restart-case.
;;;
;;; Clauses are of the form:
;;; (<restart-name> <description> <effector-arguments> <effector-body>)
;;;
;;; <restart-name> should be a valid name for a restart.
;;;
;;; <description> should be a valid description for a restart.
;;;
;;; <effector-arguments> and <effector-body> are the list of arguments and body
;;; for the effector of the restart respectively.
;;;
;;; For now, #f will be used as the interactor for every restart, signifying
;;; that they are not meant to be restarted interactively.
(define-syntax make-restart-for-clause
  (syntax-rules ()
    ((make-restart-for-clause (restart-name description effector-arguments effector-body) k)
     (%make-restart restart-name description
                    (lambda effector-arguments (k effector-body)) #f))))

(define-syntax restart-case
  (syntax-rules ()
    ((restart-case expression clause ...)
     (call-with-current-continuation
      (lambda (k)
        (parameterize ((*current-restarts*
                        (append (list (make-restart-for-clause clause k) ...)
                                (*current-restarts*))))
                      expression))))))
