#lang racket

(provide test-x860
         test-x861)

(require (for-syntax racket/syntax syntax/parse)
         rackunit
         siek
         siek/inspect)

(define-syntax (define-test-x86 stx)
  (syntax-case stx ()
    [(_ x86)
     (with-syntax ([test-x86 (format-id #'x86 "test-~a" (syntax-e #'x86))]
                   [interp-x86 (format-id #'x86 "interp-~a" (syntax-e #'x86))])
       #`(...
          (define-syntax (test-x86 stx)
            (syntax-parse stx
              [(_ name
                  (~optional (~seq #:input input:str))
                  result
                  stanza* ...)
               (with-syntax ([body
                              #'(let
                                    ([prog `(program () ,(stanza*->cfg '(stanza* ...)))])
                                  (with-check-info
                                    (['program (unquoted-printing-string (let ([port (open-output-string)])
                                                                           (write-x86 port prog)
                                                                           (get-output-string port)))])
                                    (check-equal? (interp-x86 prog) result)))])
                 #'(test-case name
                     (~?
                      (with-input input body)
                      body)))]))))]))

(define (stanza*->cfg block*)
  (map stanza->block block*))

(define (stanza->block s)
  (define info
    (if (compiler-psuedo-x86?)
        empty
        '((stack-space . 0))))
  (match s
    [`(,label ,instr* ...)
     `(,label
       .
       (block ,info
              ,@instr*))]))

(define-syntax-rule (with-input input body ...)
  (parameterize ([current-input-port (open-input-string input)])
    body
    ...))

(define-test-x86 x860)
(define-test-x86 x861)
