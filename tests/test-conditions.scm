(check (condition? (make-condition condition-type:error '() '())) => #t)

(check (condition? condition-type:error) => #f)

(check (condition/type (make-condition condition-type:file-error '() '())) =>
       condition-type:file-error)

(check (call-with-current-continuation
        (lambda (k)
          (with-condition-handler
           '()
           (lambda (condition)
             (k (restarter-tag (car (condition/restarters condition)))))
           (lambda ()
             (with-exiting-restarter 'blah "Test restarter"
                                     (lambda () (error "Error!")))))))
       => 'blah)
