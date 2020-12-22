#lang racket

(provide test-interpreter)

(require rackunit)

(define-syntax (test-interpreter stx)
  (syntax-case stx (<=)
    [(_ interp-L clause* ...)
     (let ([clause* (map (clause #'interp-L) (syntax->list #'(clause* ...)))])
       #`(test-suite (symbol->string 'interp-L) #,@clause*))]))

(define-for-syntax ((clause interp-L-stx) stx)
  (syntax-case stx (<=)
    [[exp <= input]
     (with-syntax ([interp-L interp-L-stx])
       #'(test-case (format "~a" 'exp)
           (check-equal? (with-input input (interp-L `(program () ,'exp)))
                         (with-input input exp))))]
    [exp
     (with-syntax ([interp-L interp-L-stx])
       #'(test-case (format "~a" 'exp)
           (check-equal? (interp-L `(program () ,'exp))
                         exp)))]))

(define-syntax-rule (with-input input body ...)
  (parameterize ([current-input-port (open-input-string input)])
    body ...))
