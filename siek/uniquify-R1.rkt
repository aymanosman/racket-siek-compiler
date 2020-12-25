#lang racket

(provide uniquify-R1%
         uniquify-R1)

(require "gensym.rkt"
         "raise-mismatch-error.rkt")

(define (uniquify-R1 p)
  (send (new uniquify-R1%) uniquify p))

(define uniquify-R1%
  (class object%
    (super-new)

    (define (who)
      'uniquify-R1)

    (define/public (uniquify p)
      (match p
        [`(program ,info ,e)
         `(program ,info ,((uniquify-exp '()) e))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public ((uniquify-exp env) e)
      (define recur (uniquify-exp env))
      (match e
        [(? fixnum?)
         e]
        [`(read)
         e]
        [`(- ,e)
         `(- ,(recur e))]
        [`(+ ,e0 ,e1)
         `(+ ,(recur e0) ,(recur e1))]
        [(? symbol?)
         (dict-ref env e #f)]
        [`(let ([,var ,e0]) ,e1)
         (define e0.1 (recur e0))
         (define var.1 ((current-gensym) var))
         `(let
           ([,var.1 ,e0.1])
           ,((uniquify-exp (dict-set env var var.1)) e1))]
        [_
         (raise-mismatch-error (who) 'exp e)]))))
