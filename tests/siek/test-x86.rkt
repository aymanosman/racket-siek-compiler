#lang racket

(provide test-x860
         test-x861)

(require (for-syntax racket/syntax syntax/parse)
         rackunit
         siek)

(define-syntax (define-test-x86 stx)
  (syntax-case stx ()
    [(_ x86)
     (let ()
       (define/with-syntax test-x86 (format-id #'x86 "test-~a" (syntax-e #'x86)))
       (define/with-syntax interp-x86 (format-id #'x86 "interp-~a" (syntax-e #'x86)))
       (define/with-syntax format-x86 (format-id #'x86 "format-~a" (syntax-e #'x86)))
       #`(define-syntax
          (test-x86 stx)
          (syntax-parse stx
            [(_ name
                (~optional (~seq #:input input:str))
                result
                stanza*
                (... ...))
             (with-syntax ([body
                            #'(let
                               ([prog `(program () ,(stanza*->cfg '(stanza* (... ...))))])
                               (with-check-info
                                (['program (unquoted-printing-string (format-x86 prog))])
                                (check-equal? (interp-x86 prog) result)))])
               #'(test-case
                  name
                  ((... ~?)
                   (with-input input body)
                   body)))])))]))

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
