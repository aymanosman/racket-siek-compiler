#lang racket

(provide define-interp-test-suite
         define-extended-interp-test-suite)

(require rackunit
         siek/inspect
         (for-syntax racket/syntax syntax/parse))

(define-syntax (define-interp-test-suite stx)
  (syntax-case stx (test)
    [(_ id t* ...)
     #'(define-extended-interp-test-suite id #f t* ...)]))

(define-syntax (define-extended-interp-test-suite stx)
  (syntax-case stx (test)
    [(_ id parent #:interpreter interp (test case* ...) ...)
     (with-syntax ([id-info (format-id #'id "~a-info" (syntax-e #'id))]
                   [((case* ...) ...)
                    (append (syntax->list #'((case* ...) ...))
                            (parent-test-cases #'parent))])
       #`(begin
           (define-syntax id-info (hash 'test-cases '((case* ...) ...)))
           (define-test-suite id
             (let-syntax ([test-C (test-C #'interp)])
               (test-C case* ...))
             ...)))]))

(define-for-syntax ((test-C interp-C) stx)
  (syntax-parse stx
    [(loc (~optional (~seq #:input input:str))
          (~seq #:expect result)
          stanza*
          ...)
     (with-syntax ([interp-C interp-C])
       #`(let ([prog `(program () ,(stanza*->code '(stanza* ...)))])
           (test-case (~a (C prog))
                      #,(syntax/loc #'loc
                          (check-interp interp-C prog result (~? input ""))))))]))

(define-for-syntax (parent-test-cases parent-stx)
  (local-require threading)
  (cond
    [(syntax-e parent-stx)
     (~> (format-id parent-stx "~a-info" (syntax-e parent-stx))
         (syntax-local-value _)
         (hash-ref _ 'test-cases))]
    [else
     '()]))

(define (stanza*->code _)
  (map stanza->code _))

(define stanza->code
  (match-lambda
    [(list* label tail)
     (cons label
           (let loop ([tail tail])
             (cond
               [(empty? (rest tail)) (first tail)]
               [else
                `(seq ,(first tail) ,(loop (rest tail)))])))]))

(define-check (check-interp interp p expected input)
  (define actual
    (parameterize ([current-input-port (open-input-string input)])
      (interp p)))
  (unless (equal? actual expected)
    (with-check-info (['actual actual]
                      ['expected expected])
      (fail-check))))

