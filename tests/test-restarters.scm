(check (current-restarters) => '())

(check (restarter?
        (car (with-exiting-restarter 'blah "Test" (lambda () (current-restarters)))))
       => #t)

(check (restarter-tag
        (car (with-exiting-restarter 'blah "Test" (lambda () (current-restarters)))))
       => 'blah)

(check (restarter-description
        (car (with-exiting-restarter 'blah "Test" (lambda () (current-restarters)))))
       => "Test")

;;; A restart is established in frob-alist-element that continues the
;;; computation with a given value. Then, a condition handler is bound for
;;; wrong-type-datum conditions in frob-alist that invokes this restart
;;; with the value -1. frob-alist-element will signal a wrong-type-datum
;;; condition if its argument is not a pair, and so return a -1.
(letrec ((frob-alist
          (lambda (alist)
            (bind-condition-handler (list condition-type:wrong-type-datum)
                                    (lambda (condition) (use-value -1 condition))
                                    (lambda () (map frob-alist-element alist)))))
         (frob-alist-element
          (lambda (element)
            (restarter-bind (if (pair? element)
                                (+ (cdr element) 10)
                                (error condition-type:wrong-type-datum
                                       (cons 'datum element)
                                       (cons 'type 'pair)))
                            ('use-value "Use the given value." (value) value)))))
  (check (frob-alist '((bleh . 23) foo (bar . 4))) => '(33 -1 14)))
