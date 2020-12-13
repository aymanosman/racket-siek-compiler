#lang racket

(provide uniquify-pass-R1)

(require "gensym.rkt")

(define (uniquify-pass-R1 p)
  (match p
    [`(program ,info ,e)
     `(program ,info ,((uniquify-exp '()) e))]
    [_
     (raise-mismatch-error 'uniquify-pass-R1 'top p)]))

(define ((uniquify-exp env) e)
  (define recur (uniquify-exp env))
  (match e
    [(? fixnum?) e]
    [`(read) e]
    [`(- ,e) `(- ,(recur e))]
    [`(+ ,e0 ,e1) `(+ ,(recur e0) ,(recur e1))]
    [(? symbol?) (or (dict-ref env e) e)]
    [`(let ([,var ,e0]) ,e1)
     (define e0.1 (recur e0))
     (define var.1 ((current-gensym) var))
     `(let
       ([,var.1 ,e0.1])
       ,((uniquify-exp (dict-set env var var.1)) e1))]
    [_
     (raise-mismatch-error 'uniquify-pass-R1 'exp e)]))
