;Record type representing a restart object.
;A restart object consists of a name, description, effector and interactor.
;
;%make-restart : name description effector interactor
;is a private function that constructs and returns a new restart object.
;
;restart? : object
;returns #f if and only if object is not a restart.
;
;restart/name : restart
;returns restart's name. A name can be any object, but is usually a
;symbol or #f.
;
;restart/description : restart
;returns restart's description.
;
;restart/effector : restart
;returns the effector encapsulated in restart. An effector can be used to
;continue, abort, or restart the computation it's restart object was created in.
;
;restart/interactor : restart
;returns the interactor encapsulated in the given restart. An interactor must
;be a function that takes no arguments and returns the number of values restart's
;effector expects to recieve.
(define-record-type restart
  (%make-restart name description effector interactor)
  restart?
  (name restart/name)
  (description restart/description)
  (effector restart/effector)
  (interactor restart/interactor))

;A list of the restarts in effect for the current dynamic extent
(define *current-restarts* (make-parameter '()))

;Returns a list of the restarts currently in effect
(define (bound-restarts) (list-copy (*current-restarts*)))

;Calls thunk with a restart encapsulating the given name, description,
;and interactor in effect during it's execution.
(define (with-restart name description effector interactor thunk)
  (parameterize ((*current-restarts*
                  (cons (%make-restart name description effector interactor)
                        (*current-restarts*))))
    (thunk)))

;Calls thunk with a restart encapsulating the given name and description
;and with an effector that continues the current computation, returning
;no values. Its interactor does not collect arguments and returns no values.
(define (with-simple-restart name description thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restart name description continuation values thunk))))

;Calls restart's effector using args as arguments.
(define (invoke-restart restart . args)
  (if (restart? restart)
      (apply (restart/effector restart) args)
      (error "invalid restart argument"
             `(INVOKE-RESTART ,restart ,@args))))

;First calls the interactor encapsulated in restart to interactively gather
;the arguments needed for restart's effector. It then calls the effector,
;passing these arguments to it.
(define (invoke-restart-interactively restart)
  (if (restart? restart)
      (call-with-values (restart/interactor restart)
        (restart/effector restart))
      (error "invalid restart argument"
             `(INVOKE-RESTART-INTERACTIVELY ,restart))))

;Returns the first restart object named name in restarts. restarts can be
;either a list of restart objects, or a condition, in which case the list of
;restarts given by condition/restarts is searched. If restarts is not supplied,
;the list return by (bound-restarts) is searched.
;
;When used in a condition handler, find-restart is usually passed the name of a
;particular restart and the condition object that has been signalled. In this
;way the handler finds only restarts that were available when the condition was
;created (usually the same as when it was signalled).
(define* (find-restart name (restarts #f))
  (let ((restart-list
         (cond ((not restarts) (bound-restarts))
               ((condition? restarts) (condition/restarts restarts))
               ((list? restarts) restarts)
               (else '()))))
    (find (lambda (x) (equal? (restart/name x) name)) restart-list)))
