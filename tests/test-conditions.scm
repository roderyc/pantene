(check (condition? (make-condition condition-type:error #f '() '())) => #t)

(check (condition? (ignore-errors (lambda () (error "Error!")))) => #t)

(check (condition? condition-type:error) => #f)

(check (not (condition/error? (make-condition condition-type:error #f '() '()))) => #f)

(check (condition/error? (make-condition condition-type:serious-condition #f '() '())) => #f)

(check (condition/type (make-condition condition-type:file-error #f '() '())) =>
       condition-type:file-error)

(check (restart/name
        (car (condition/restarts
              (with-simple-restart 'blah "Test restart"
                                   (lambda () (ignore-errors
                                               (lambda () (error "Error!"))))))))
       => 'blah)

(check (condition/report-string
        (ignore-errors (lambda () (error "Error!" 1 2 3))))
       => "Error! : 1 2 3")
