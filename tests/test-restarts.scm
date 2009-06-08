(check (bound-restarts) => '())

(check (restart?
        (car (with-simple-restart 'blah "Test" (lambda () (bound-restarts)))))
       => #t)

(check (restart/name
        (car (with-simple-restart 'blah "Test" (lambda () (bound-restarts)))))
       => 'blah)

(check (restart/description
        (car (with-simple-restart 'blah "Test" (lambda () (bound-restarts)))))
       => "Test")

;;; A restart is established in frob-alist-element that continues the
;;; computation with a given value. Then, a condition handler is bound for
;;; wrong-type-datum conditions in frob-alist that invokes this restart
;;; with the value -1. frob-alist-element will signal a wrong-type-datum
;;; condition if its argument is not a pair, and so return a -1.
(letrec ((frob-alist (lambda (alist)
                       (bind-condition-handler (list condition-type:wrong-type-datum)
                                               (lambda (condition) (use-value -1 condition))
                                               (lambda () (map frob-alist-element alist)))))
         (with-use-value-restart
          (lambda (thunk)
            (call-with-current-continuation
             (lambda (k)
               (with-restart 'use-value
                             "Return the given value for the current computatoin"
                             k
                             values
                             thunk)))))
         (frob-alist-element (lambda (element)
                               (with-use-value-restart
                                (lambda ()
                                  (if (pair? element)
                                      (+ (cdr element) 10)
                                      (error condition-type:wrong-type-datum
                                             (cons 'datum element)
                                             (cons 'type 'pair))))))))
  (check (frob-alist '((bleh . 23) foo (bar . 4))) => '(33 -1 14)))
