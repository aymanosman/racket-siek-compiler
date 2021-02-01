#lang racket

(provide define-compiler-test-suite
         define-extended-compiler-test-suite)

(require rackunit
         (for-syntax racket/syntax)
         "check-pass.rkt")

(define-syntax (define-compiler-test-suite stx)
  (syntax-case stx (->)
    [(_ id
        #:compiler compiler
        #:signature signature
        case* ...)
     (let ()
       (define/with-syntax id-info (format-id #'id "~a-info" (syntax-e #'id)))
       #'(begin
           (define-syntax id-info (hash 'test-cases '(case* ... )))
           (define-test-suite id
             (test-compiler compiler signature case* ...))))]))

(define-syntax (define-extended-compiler-test-suite stx)
  (syntax-case stx ()
    [(_ id parent
        #:compiler compiler
        #:signature signature
        e* ...)
     (let ()
       (with-syntax ([id-info (format-id #'id "~a-info" (syntax-e #'id))]
                     [(case* ...)
                      (append (syntax->list #'(e* ...))
                              (hash-ref (syntax-local-value (format-id #'parent
                                                                       "~a-info"
                                                                       (syntax-e #'parent)))
                                        'test-cases))])
         #`(begin
             (define-syntax id-info (hash 'test-cases '(case* ...)))
             (define-test-suite id
               (test-compiler compiler signature case* ...)))))]))
