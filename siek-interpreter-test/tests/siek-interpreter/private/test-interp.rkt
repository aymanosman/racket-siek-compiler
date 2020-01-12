#lang racket

(provide test-interp*)

(require "with-input.rkt")

(require rackunit)

(define-syntax (test-interp* stx)
  (syntax-case stx (<=)
    [(_ interp-L) #'(void)]
    [(_ interp-L [exp <= input] clause* ...)
      #`(begin
          (check-equal?
            (with-input input
              (interp-L `(program () ,'exp)))
            (with-input input exp))
          (test-interp* interp-L clause* ...))]
    [(_ interp-L exp clause* ...)
      #`(begin
          (check-equal? (interp-L `(program () ,'exp))
            exp)
          (test-interp* interp-L clause* ...))]))
