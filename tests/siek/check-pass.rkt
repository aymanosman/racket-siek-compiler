#lang racket

(provide check-pass
         check-pass*)

(require rackunit
         (for-syntax racket/syntax
                     syntax/parse))

(require siek)

(define-syntax (check-pass stx)
  (syntax-parse stx
    #:literals (->)
    [(check-pass pass (L0:id -> L1:id) e)
     (with-syntax ([interp-L0 (format-id #'here "interp-~a" (syntax-e #'L0))]
                   [interp-L1 (format-id #'here "interp-~a" (syntax-e #'L1))])
       #'(check-pass-fun pass interp-L0 interp-L1 e))]))

(define-syntax (check-pass* stx)
  (syntax-parse stx
    #:literals (->)
    [(check-pass* pass spec e)
     #'(check-pass pass spec e)]
    [(check-pass* pass spec e0 e* ...)
     #`(begin
         (check-pass pass spec e0)
         (check-pass* pass spec e* ...))]))

(define-check (check-pass-fun pass interp-L0 interp-L1 e)
  (define p `(program () ,e))
  (unless (equal? (interp-L1 (pass p))
                  (interp-L0 p))
    (fail-check)))
