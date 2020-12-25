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
       (define/with-syntax parent-info (format-id #'parent "~a-info" (syntax-e #'parent)))
       (define/with-syntax id-info (format-id #'id "~a-info" (syntax-e #'id)))
       (define parent-test-cases (hash-ref (syntax-local-value #'parent-info) 'test-cases))
       (define/with-syntax (case* ...) (append (syntax->list #'(e* ...)) parent-test-cases))
       #`(begin
           (define-syntax id-info (hash 'meta '(case* ...)))
           (define-test-suite id
             (test-compiler compiler signature case* ...))))]))
