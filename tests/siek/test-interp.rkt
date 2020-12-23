#lang racket

(provide test-interp)

(require rackunit
         (for-syntax racket/syntax))

(define-syntax (test-interp stx)
  (syntax-case stx (<=)
    [(_ L clause* ...)
     (with-syntax ([interp-L (format-id #'L "interp-~a" (syntax-e #'L))])
       (let ([clause* (map (clause #'interp-L) (syntax->list #'(clause* ...)))])
         #`(test-suite (symbol->string 'interp-L) #,@clause*)))]))

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
