#lang racket

(provide uniquify-pass-R1)

(require "gensym.rkt")

(define (uniquify-pass-R1 p)
  (match p
    [`(program ,info ,e)
     `(program ,info ,((uniquify-exp '()) e))]
    [_
     ((current-R1-mismatch-handler) 'top p)]))

(define ((uniquify-exp env) e)
  (define recur (uniquify-exp env))
  (match e
    [(? fixnum?) e]
    [`(read) e]
    [`(- ,e) `(- ,(recur e))]
    [`(+ ,e0 ,e1) `(+ ,(recur e0) ,(recur e1))]
    [(? symbol?) (or (lookup env e) e)]
    [`(let ([,var ,e0]) ,e1)
     (define e0.1 (recur e0))
     (define var.1 ((current-gensym) var))
     `(let ([,var.1 ,e0.1])
        ,((uniquify-exp (extend env (cons var var.1))) e1))]
    [_
     ((current-R1-mismatch-handler) 'exp e)]))

(define current-R1-mismatch-handler
  (make-parameter
   (lambda (kind term)
     (raise-arguments-error 'uniquify-pass-R1 "failed to match"
                            "kind" kind
                            "term" term))))

;; Aux

(define (extend env entry)
  (cons entry env))

(define (lookup env var)
  (match (assoc var env)
    [(cons _ sub) sub]
    [else #f]))
