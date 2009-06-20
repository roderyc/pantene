;;;; Restarters

;;; This is a modified version of the SRFI proposal for restarting conditions
;;; by Taylor Campbell.
;;;
;;; Restarters provide a mechanism to encapsulate the information necessary to
;;; restart a computation. In order to recover flexibly from exceptional
;;; situations, the signaler must provide ways for the handler to restart
;;; the computation, some of which may require extra input. The decision of
;;; which method of recovery to choose can be left to a physical user, who
;;; maybe be prompted for the input. Restarters allow the handler or user
;;; to communicate a decision to the signaler.
;;;
;;; A restarter is an object of a new disjoint type with four fields:
;;;
;;; tag - an identifying tag, usually a symbol, though it may be anything
;;;
;;; description - a string that describes the method of recovery that
;;;   this restarter invokes; by convention, a complete English sentence,
;;;   starting with a capital letter and ending with a full-stop, in the
;;;   imperative
;;;
;;; invoker - a procedure that actually performs the recovery, possibly
;;;   given some arguments
;;;
;;; interactor - a procedure of zero arguments that prompts the user for
;;;   some inputs, whose return values are all passed to the invoker when
;;;   the restarter is invoked interactively; or #F, if the restarter is
;;;   non-interactive
;;;
;;; Restarters are constructed explicitly with MAKE-RESTARTER or implicitly
;;; with CALL-WITH-RESTARTER, CALL-WITH-INTERACTIVE-RESTARTER, etc.  The tag
;;; and description fields are explicitly accessible; the invoker and
;;; interactor fields are used only through the procedures RESTART and
;;; RESTART-INTERACTIVELY.
;;;
;;; A field is allocated in the dynamic environment for a list of currently
;;; available restarters in a particular dynamic context.  This list can be
;;; obtained with the CURRENT-RESTARTERS procedure; the WITH-RESTARTER
;;; procedure pushes a restarter onto the list for a particular dynamic
;;; extent, as do a number of other wrappers around it.
;;;

;;;; Restarter Operations

;;; (MAKE-RESTARTER tag description invoker interactor) -> restarter
;;; (RESTARTER? obj) -> boolean
;;; (RESTARTER-TAG restarter) -> object
;;; (RESTARTER-DESCRIPTION restarter) -> string
;;;   MAKE-RESTARTER constructs a restarter; RESTARTER? is the restarter
;;;   disjoint type predicate; and RESTARTER-TAG & RESTARTER-DESCRIPTION
;;;   access the tag & description fields, respectively, of restarters.
;;;
;;; (RESTART restarter arg ...) -> values (may not return)
;;; (RESTART-INTERACTIVELY restarter) -> values (may not return)
;;;   RESTART calls a restarter's invoker  procedure with the given
;;;   arguments.  RESTART-INTERACTIVELY calls a restarter's interactor
;;;   procedure with zero arguments, and then passes the values it returned
;;;   to the restarter's invoker.  The RESTARTER argument may be a
;;;   restarter or any object such that there is a restarter in the list of
;;;   current restarters whose tag is the object; that restarter's invoker
;;;   or interactor is used.
;;;
;;; (CURRENT-RESTARTERS) -> list
;;; (WITH-RESTARTER restarter thunk) -> values
;;; (FIND-RESTARTER tag [list]) -> restarter or #F
;;;   CURRENT-RESTARTERS returns a list of the current restarters in the
;;;   nested order in which they were successively pushed.  It is an error
;;;   to modify this list.  WITH-RESTARTER calls THUNK, for whose dynamic
;;;   extent RESTARTER is pushed onto the list of current restarters, and
;;;   returns the values returned by THUNK.  FIND-RESTARTER searches in the
;;;   current restarter list, or LIST if it is supplied, for the most
;;;   recently pushed restarter whose tag is equal to the given tag in the
;;;   sense of EQV?. If there is no such restarter in the list, FIND-RESTARTER
;;;   returns #f. LIST can be either a list of restarters or a condition,
;;;   in which case the list of restarters given by condition-restarters is
;;;   searched.
;;;
;;; (WITH-EXITING-RESTARTER tag description thunk) -> values
;;;   WITH-EXITING-RESTARTER creates an interactive restarter with the
;;;   given tag & description, whose invoker returns zero values to the
;;;   call to WITH-EXITING-RESTARTER and whose interactor returns zero
;;;   values; it then calls THUNK, for whose dynamic extent it is added to
;;;   the list of current restarters.

;;;; Restarter protocols

;;; There are several tags that, by convention, hold to particular
;;; behavior protocols. These tags are simply symbols. six simple procedures
;;; are provided for use by condition handling code to invoke restarters
;;; adhering to these protocols. Unless otherwise specified, if one of these
;;; procedures is unable to find its corresponding restarter, it returns
;;; immediately with an unspecified value. Each of these procedures accepts an
;;; optional argument LIST; the same described by FIND-RESTARTER.
;;;
;;; ABORT
;;; (ABORT [list])
;;;   Completely aborts the computation. ABORT restarters' invokers accept
;;;   zero arguments; if interactive, their interactors typically simply
;;;   return zero values for the invoker. The corresponding procedure
;;;   signals a condition of type condition-type:no-such-restarter if there
;;;   is no restarter with the tag ABORT in the searched list.
;;;
;;; IGNORE
;;; (IGNORE [list])
;;;   Ignores the condition and proceeds. IGNORE restarters' invokers
;;;   accept zero arguments; if interactive, their interactors typically
;;;   simply return zero values for the invoker.
;;;
;;; MUFFLE-WARNING
;;; (MUFFLE-WARNING [list])
;;;   Continues the computation beyond the point at which any warning message
;;;   resulting from the condition would be presented to the user. MUFFLE-WARNING
;;;   restarters' invokers accept zero arguments; if interactive, their
;;;   interactors typically simply return zero values to the invoker.
;;;   would be presented to the user. The corresponding procedure signals a
;;;   condition of type condition-type:no-such-restarter if there is no
;;;   restarter with the tag MUFFLE-WARNING in the searched list.
;;;
;;; RETRY
;;; (RETRY [list])
;;;   Simply retries a whole computation from a certain point, with no
;;;   explicitly altered inputs. Some implicit environmental changes are
;;;   expected to have taken place. RETRY restarters' invokers accept zero
;;;   arguments, and their interactors typically simply return zero values
;;;   for the invoker.
;;;
;;; USE-VALUE
;;; (USE-VALUE value [list])
;;;   Retries a computation with a given input value substituted for some
;;;   invalid value in the original input. USE-VALUE restarters' invokers
;;;   accept one argument, the new value to substitute; if interactive,
;;;   their interactors prompt the user for the new value to substitute.
;;;
;;; STORE-VALUE
;;; (STORE-VALUE value [list])
;;;   STORE-VALUE restarters are in every respect like USE-VALUE restarters
;;;   except that they are meant to store the input value somewhere, so
;;;   that the old one is completely replaced, rather than just use the
;;;   input value temporarily and possibly accidentally reuse the old
;;;   value and signal another error.

;;;; Restarter Syntax

;;; (RESTARTER-BIND <expression> <clause> ...)
;;; Evaluates <expression> in a dynamic environment where restarters represented
;;; by each <clause> are in effect. If none of the restarters are invoked by
;;; <expression>, any and all values it returns are returned by RESTARTER-BIND.
;;; If a restarter is invoked, the invoker of that restarter is evaluated and
;;; any values it returns are returned by RESTARTER-BIND.
;;;
;;; Clauses are of the form:
;;; (<tag> <description> <invoker-arguments> <invoker-body>)
;;;
;;; <tag> should be a valid tag for a restarter.
;;;
;;; <description> should be a valid description for a restarter.
;;;
;;; <invoker-arguments> and <invoker-body> are the list of arguments and body
;;; for the invoker of the restarter respectively.
;;;
;;; For now, none of the restarters established by RESTARTER-BIND will be
;;; interactive. #f will be used as their interactor , signifying that they are
;;; not meant to be restarted interactively.

(define-record-type restarter
  (make-restarter tag description invoker interactor)
  restarter?
  (tag restarter-tag)
  (description restarter-description)
  (invoker restarter-invoker)
  (interactor restarter-interactor))

(define current-restarters (make-parameter '()))

(define (with-restarter tag description invoker interactor thunk)
  (parameterize ((current-restarters
                  (cons (make-restarter tag description invoker interactor)
                        (current-restarters))))
                (thunk)))

(define (with-exiting-restarter tag description thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restarter tag description
                     (lambda () (continuation))
                     (lambda ()  (values)) thunk))))

(define (restart spec . args)
  (let ((win (lambda (r) (apply (restarter-invoker r) args))))
    (cond ((restarter? spec) (win spec))
          ((find-restarter spec) => win)
          (else (error "invalid restarter specifier argument"
                       `(RESTART ,spec ,@args))))))

(define (restart-interactively spec)
  (let ((win (lambda (r)
               (if (restarter-interactor r)
                   (call-with-values (restarter-interactor r)
                     (restarter-invoker r))))))
    (cond ((restarter? spec) (win spec))
          ((find-restarter spec) => win)
          (else (error "invalid restarter specifier argument"
                       `(RESTART-INTERACTIVELY ,spec))))))

(define* (find-restarter tag (restarters #f))
  (let ((restarter-list
         (cond ((not restarters) (current-restarters))
               ((condition? restarters) (condition-restarters restarters))
               ((pair? restarters) restarters)
               (else '()))))
    (find (lambda (x) (eqv? (restarter-tag x) tag)) restarter-list)))

(define* (abort (restarters #f))
  (let ((restarter (find-restarter 'abort restarters)))
    (if restarter
        (restart restarter)
        (error:no-such-restarter 'abort))))

(define* (continue (restarters #f))
  (let ((restarter (find-restarter 'continue restarters)))
    (if restarter
        (restart restarter))))

(define* (muffle-warning (restarters #f))
  (let ((restarter (find-restarter 'muffle-warning restarters)))
    (if restarter
        (restart restarter)
        (error:no-such-restarter 'muffle-warning))))

(define* (retry (restarters #f))
  (let ((restarter (find-restarter 'retry restarters)))
    (if restarter
        (restart restarter))))

(define* (store-value value (restarters #f))
  (let ((restarter (find-restarter 'store-value restarters)))
    (if restarter
        (restart restarter value))))

(define* (use-value value (restarters #f))
  (let ((restarter (find-restarter 'use-value restarters)))
    (if restarter
        (restart restarter value))))

(define-syntax restarter-clause
  (syntax-rules ()
    ((restarter-clause (tag description invoker-arguments invoker-body) k)
     (make-restarter tag description
                     (lambda invoker-arguments (k invoker-body)) #f))))

(define-syntax restarter-bind
  (syntax-rules ()
    ((restarter-bind expression clause ...)
     (call-with-current-continuation
      (lambda (k)
        (parameterize ((current-restarters
                        (append (list (restarter-clause clause k) ...)
                                (current-restarters))))
                      expression))))))
