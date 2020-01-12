#lang racket

(provide interp-R1)

(require racket/fixnum)

(define (interp-R1 p)
  (match p
    [`(program () ,e) ((interp-R1-exp '()) e)]
    [_
      (raise-arguments-error 'interp-R1 "failed match"
                             "kind" 'top
                             "term" p)]))

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
    [(? symbol?) (lookup-R1 env e)]
    [`(let ([,x ,e0]) ,e1)
     ((interp-R1-exp (extend-R1 env (cons x (recur e0)))) e1)]))

(define (lookup-R1 env var)
  (match (assoc var env)
    [(cons _ value) value]
    [_ (raise-arguments-error var "undefined")]))

(define (extend-R1 env entry)
  (cons entry env))
