#lang racket

(provide test-compiler)

(require rackunit
         (for-syntax racket/syntax
                     syntax/parse))

(require siek)

(define-syntax (pass-case stx)
  (syntax-parse stx
    #:literals
    (-> <=)
    [(_ pass (L0:id -> L1:id) [e <= input])
     (with-syntax ([interp-L0 (format-id #'here "interp-~a" (syntax-e #'L0))] ;; TODO #'here -> #'L0
                   [interp-L1 (format-id #'here "interp-~a" (syntax-e #'L1))])
       #'(test-case
          (format "~a" 'e)
          (check-pass pass interp-L0 interp-L1 'e input)))]
    [(_ pass signature e)
     #'(pass-case pass signature [e <= ""])]))

(define-syntax (test-compiler stx)
  (syntax-parse stx
    #:literals
    (->)
    [(_ pass spec e)
     #'(pass-case pass spec e)]
    [(_ pass spec e0 e* ...)
     #`(begin
         (pass-case pass spec e0)
         (test-compiler pass spec e* ...))]))

(define-check (check-pass pass interp-L0 interp-L1 expr input)
  (define p `(program () ,expr))
  (define capture-output (open-output-string))
  (define expected (parameterize ([current-input-port (open-input-string input)])
                     (interp-L0 p)))
  (define compiled (parameterize ([current-output-port capture-output])
                     (pass p)))
  (define actual (parameterize ([current-input-port (open-input-string input)])
                   (interp-L1 compiled)))
  (unless (equal? actual expected)
    (with-check-info (['actual actual]
                      ['expected expected]
                      ['passes (unquoted-printing-string (get-output-string capture-output))])
      (fail-check))))
