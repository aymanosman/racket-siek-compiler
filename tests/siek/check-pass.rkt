#lang racket

(provide check-pass
         (rename-out [check-pass* test-compiler])
         check-pass*)

(require rackunit
         (for-syntax racket/syntax
                     syntax/parse))

(require siek)

(define-syntax (check-pass stx)
  (syntax-parse stx
    #:literals
    (->)
    [(_ pass (L0:id -> L1:id) e)
     (with-syntax ([interp-L0 (format-id #'here "interp-~a" (syntax-e #'L0))] ;; TODO #'here -> #'L0
                   [interp-L1 (format-id #'here "interp-~a" (syntax-e #'L1))])
       #'(test-case
          (format "~a" 'e)
          (check-pass-fun pass interp-L0 interp-L1 'e)))]))

(define-syntax (check-pass* stx)
  (syntax-parse stx
    #:literals
    (->)
    [(_ pass spec e)
     #'(check-pass pass spec e)]
    [(_ pass spec e0 e* ...)
     #`(begin
        (check-pass pass spec e0)
        (check-pass* pass spec e* ...))]))

(define-check (check-pass-fun pass interp-L0 interp-L1 e)
  (define p `(program () ,e))
  (define capture-output (open-output-string))
  (define actual (parameterize ([current-output-port capture-output])
                    (interp-L1 (pass p))))
  (define expected (interp-L0 p))
  (unless (equal? actual expected)
    (with-check-info (['actual actual]
                      ['expected expected]
                      ['passes (unquoted-printing-string (get-output-string capture-output))])
      (fail-check))))
