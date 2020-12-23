#lang racket

(provide test-typecheck-fail)

(require rackunit
         (for-syntax racket/syntax))

(define-syntax (test-typecheck-fail stx)
  (syntax-case stx (<=)
    [(_ L clause* ...)
     (with-syntax ([typecheck-L (format-id #'L "typecheck-~a" (syntax-e #'L))])
       (let ([clause* (map (clause #'typecheck-L) (syntax->list #'(clause* ...)))])
         #`(test-suite
            (symbol->string 'typecheck-L)
            #,@clause*)))]))

(define-for-syntax ((clause typecheck-L-stx) exp-stx)
  (with-syntax ([typecheck-L typecheck-L-stx]
                [exp exp-stx])
    #'(test-case (format "~a" 'exp)
        (check-exn
         #rx"type mismatch"
         (thunk
          (typecheck-L `(program () ,'exp)))))))
