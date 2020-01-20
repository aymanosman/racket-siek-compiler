#lang racket

(provide interp-C0)

(require racket/fixnum)

(define (interp-C0 p)
  (match p
    [`(program ,info ((start . ,tail)))
     (interp-tail '() tail)]
     [_
       (raise-arguments-error 'interp-C0 "failed match"
                              "kind" 'top
                              "term" p)]))

(define (interp-tail env t)
  (match t
    [`(return ,e) (interp-exp env e)]
    [`(seq ,s ,t) (interp-tail (interp-stmt env s) t)]
    [_
      (raise-arguments-error 'interp-C0 "failed match"
                             "kind" 'tail
                             "term" t)]))

(define (interp-stmt env s)
  (match s
    [`(assign ,v ,e) (extend env v (interp-exp env e))]
    [_
      (raise-arguments-error 'interp-C0 "failed match"
        "kind" 'stmt
        "term" s)]))

(define (interp-exp env e)
  (match e
    [(? symbol?) (lookup env e)]
    [(? fixnum?) e]
    [`(- ,a) (fx- 0 (interp-exp env a))]
    [`(+ ,a0 ,a1) (fx+ (interp-exp env a0) (interp-exp env a1))]
    [`(read) (read-int)]
    [_
      (raise-arguments-error 'interp-C0 "failed match"
        "kind" 'exp
        "term" e)]))

;; Aux

(define (read-int)
  (match (read)
    [(? fixnum? r) r]
    [other
      (raise-argument-error 'interp-C0 "integer?" other)]))

(define (lookup env var)
  (match (assoc var env)
    [(cons _ value) value]
    [_
      (raise-arguments-error var "undefined variable")]))

(define (extend env name value)
  (cons (cons name value) env))