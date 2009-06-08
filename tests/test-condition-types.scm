;;; condition-type/name should simply return the symbol representing
;;; the name of the type.
(check (condition-type/name condition-type:file-error) => 'file-error)

(check (condition-type/name condition-type:serious-condition) => 'serious-condition)

;;; The generalizations for condition-type/generalisations argument
;;; should include itself and all of its direct ancestor's
;;; generalizations.
(check (condition-type/generalizations condition-type:file-error) =>
       (list condition-type:file-error
             condition-type:error
             condition-type:serious-condition))

(check (condition-type/generalizations condition-type:serious-condition) =>
       (list condition-type:serious-condition))

;;; field-names for a condition type should be a list of its field names or
;;; the empty list if it has no fields.
(check (condition-type/field-names condition-type:simple-error) =>
       '(message irritants))

(check (condition-type/field-names condition-type:serious-condition) => '())

;;; condition/report-string calls the procedure returned by condition-type/reporter
;;; so it's used for these tests.
(check (condition/report-string (ignore-errors (lambda () (error "Error!"
                                                                 1 2 3 4 5 6))))
       => "Error! : 1 2 3 4 5 6")

(check (condition/report-string (ignore-errors
                                 (lambda () (error condition-type:wrong-type-datum
                                                   (cons 'datum 'blah)
                                                   (cons 'type "integer")))))
       => "The object blah is not an integer.")

(check (condition/report-string (ignore-errors
                                 (lambda () (error condition-type:wrong-type-argument
                                                   (cons 'datum 'blah)
                                                   (cons 'type "integer")
                                                   (cons 'operator 'add)
                                                   (cons 'operand 'x)))))
       => "The object blah, passed as the argument x to add, is not an integer.")

(check (condition/report-string (ignore-errors
                                 (lambda () (error condition-type:bad-range-argument
                                                   (cons 'datum 23)
                                                   (cons 'operator 'indexer)
                                                   (cons 'operand 'index)))))
       => "The object 23, passed as the argument index to indexer, is not in the correct range.")

(check (condition/report-string
        (ignore-errors (lambda () (error condition-type:file-error
                                         (cons 'filename "foo.scm")))))
       => "File error associated with: foo.scm")
