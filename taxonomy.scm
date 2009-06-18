(define (report message field-name)
  (lambda (condition port)
     (display message port)
     (display (access-condition condition field-name) port)))

(define condition-type:serious-condition
  (make-condition-type 'serious-condition #f '() #f))

(define condition-type:error
  (make-condition-type
   'error
   condition-type:serious-condition
   '()
   #f))

(define condition-type:simple-error
  (make-condition-type
   'simple-error
   condition-type:error
   '(message irritants)
   (lambda (condition port)
     (display (access-condition condition 'message) port)
     (if (not (null? (access-condition condition 'irritants)))
         (begin
           (display " :" port)
           (for-each (lambda (x)
                       (display " " port)
                       (display x port))
                     (access-condition condition 'irritants)))))))

(define condition-type:illegal-datum
  (make-condition-type
   'illegal-datum
   condition-type:error
   '(datum)
   (lambda (condition port)
     (display "The object " port)
     (display (access-condition condition 'datum) port)
     (display " has been found in an inappropriate context."
              port))))

(define condition-type:datum-out-of-range
  (make-condition-type
   'datum-out-of-range
   condition-type:illegal-datum
   '()
   (lambda (condition port)
     (display "The object " port)
     (display (access-condition condition 'datum) port)
     (display " is not in the correct range." port))))

(define write-type-description
  (let ((char-set:vowels
         (char-set #\a #\e #\i #\o #\u)))
    (lambda (condition port)
      (let ((type (access-condition condition 'type)))
        (if (string? type)
            (begin
              (if (not (or (string-null? type)
                           (string-prefix-ci? "a " type)
                           (string-prefix-ci? "an " type)))
                  (display
                   (if (char-set-contains? char-set:vowels
                                           (string-ref type 0))
                       "an "
                       "a ")
                   port))
              (display type port))
            (display "the correct type" port))))))

(define	write-operand-description
  (lambda (condition port)
    (let ((operator (access-condition condition 'operator))
          (operand (access-condition condition 'operand)))
      (if (or (symbol? operator)
              (procedure? operator))
          (begin
            (display ", passed as the argument " port)
            (display operand port)
            (display " to " port)
            (display operator port)
            (display "," port))))))

(define condition-type:wrong-type-datum
  (make-condition-type
   'wrong-type-datum
   condition-type:illegal-datum
   '(type)
   (lambda (condition port)
     (display "The object " port)
     (display (access-condition condition 'datum) port)
     (display " is not " port)
     (write-type-description condition port)
     (display "." port))))

(define condition-type:wrong-type-argument
  (make-condition-type
   'wrong-type-argument
   condition-type:wrong-type-datum
   '(operator operand)
   (lambda (condition port)
     (display "The object " port)
     (display (access-condition condition 'datum) port)
     (write-operand-description condition port)
     (display " is not " port)
     (write-type-description condition port)
     (display "." port))))

(define condition-type:bad-range-argument
  (make-condition-type
   'bad-range-argument
   condition-type:datum-out-of-range
   '(operator operand)
   (lambda (condition port)
     (display "The object " port)
     (display (access-condition condition 'datum) port)
     (write-operand-description condition port)
     (display " is not in the correct range." port))))

(define condition-type:control-error
  (make-condition-type
   'control-error
   condition-type:error
   '()
   "Control error."))

(define condition-type:no-such-restart
  (make-condition-type
   'no-such-restart
   condition-type:control-error
   '(name)
   (lambda (condition port)
     (display "The restart named " port)
     (display (access-condition condition 'NAME) port)
     (display " is not bound." port))))

(define condition-type:file-error
  (make-condition-type
   'file-error
   condition-type:error
   '(filename)
   (report "File error associated with: " 'filename)))

(define condition-type:primitive-procedure-error
  (make-condition-type
   'primitive-procedure-error
   condition-type:error
   '(operator operands)
   (report "This is a primitive procedure error: " 'operator)))


