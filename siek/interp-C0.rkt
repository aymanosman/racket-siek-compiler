#lang racket

(provide interp-C0)

(require racket/fixnum
         "raise-mismatch-error.rkt")

(define (interp-C0 p)
  (match p
    [`(program ,info ((start . ,tail)))
     (interp-tail '() tail)]
    [_
     (raise-mismatch-error 'interp-C0 'top p)]))

(define (interp-tail env t)
  (match t
    [`(return ,e) (interp-exp env e)]
    [`(seq ,s ,t) (interp-tail (interp-stmt env s) t)]
    [_
     (raise-mismatch-error 'interp-C0 'tail t)]))

(define (interp-stmt env s)
  (match s
    [`(assign ,v ,e) (dict-set env v (interp-exp env e))]
    [_
     (raise-mismatch-error 'interp-C0 'stmt s)]))

(define (interp-exp env e)
  (match e
    [(? symbol?) (dict-ref env e)]
    [(? fixnum?) e]
    [`(- ,a) (fx- 0 (interp-exp env a))]
    [`(+ ,a0 ,a1) (fx+ (interp-exp env a0) (interp-exp env a1))]
    [`(read)
     (match (read)
       [(? fixnum? r) r]
       [other
        (raise-argument-error 'interp-C0 "fixnum?" other)])]
    [_
     (raise-mismatch-error 'interp-C0 'exp e)]))
