#lang racket

(provide arg)

(define-match-expander arg
  (lambda (stx)
    (syntax-case stx ()
      ((_ p)
       #'(or
          `(var ,p)
          `(reg ,p))))))
