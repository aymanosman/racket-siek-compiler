#lang racket

(provide interp-R1)

(require racket/fixnum
         "raise-mismatch-error.rkt")

(define (interp-R1 p)
  (match p
    [`(program () ,e) ((interp-R1-exp '()) e)]
    [_
     (raise-mismatch-error 'interp-R1 'top p)]))

(define ((interp-R1-exp env) e)
  (define recur (interp-R1-exp env))
  (match e
    [(? fixnum?) e]
    [`(read)
     (let ([r (read)])
       (cond
         [(fixnum? r) r]
         [else (raise-argument-error 'interp-R1 "integer?" r)]))]
    [`(- ,e) (fx- 0 (recur e))]
    [`(+ ,e0 ,e1) (fx+ (recur e0) (recur e1))]
    [(? symbol?) (dict-ref env e)]
    [`(let ([,x ,e0]) ,e1)
     ((interp-R1-exp (dict-set env x (recur e0))) e1)]
    [_
     (raise-mismatch-error 'interp-R1 'exp e)]))
