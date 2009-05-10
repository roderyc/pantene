(define (report message field-name)
  (lambda (condition port)
     (display message port)
     (newline port)
     (display (access-condition condition field-name) port)
     (newline port)))

(define condition-type:serious-condition
  (make-condition-type 'SERIOUS-CONDITION #f '() #f))

(define condition-type:error
  (make-condition-type
   'ERROR
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
           (display " : " port)
           (display (access-condition condition 'irritants) port))))))

(define condition-type:file-error
  (make-condition-type
   'FILE-ERROR
   condition-type:error
   '(FILENAME)
   (report "This is a file error" 'filename)))

(define condition-type:primitive-procedure-error
  (make-condition-type
   'PRIMITIVE-PROCEDURE-ERROR
   condition-type:error
   '(OPERATOR OPERANDS)
   (report "This is a primitive procedure error" 'operator)))


