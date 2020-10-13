#lang racket

(provide remove-complex-opera*-pass-R1)

(require "gensym.rkt")

(define (remove-complex-opera*-pass-R1 p)
  (match p
    [`(program () ,e)
     `(program () ,(rco-exp e))]
    [_
     ((current-R1-mismatch-handler) 'top p)]))

(define (rco-exp e)
  (match e
    [(? fixnum?) e]
    [`(read) e]
    [`(- ,e0)
     (define-values (v0 env0) (rco-arg e0))
     (add-lets env0 `(- ,v0))]
    [`(+ ,e0 ,e1)
     (define-values (v0 env0) (rco-arg e0))
     (define-values (v1 env1) (rco-arg e1))
     (add-lets (append env1 env0) `(+ ,v0 ,v1))]
    [(? symbol?) e]
    [`(let ([,x ,e0]) ,e1)
     `(let ([,x ,(rco-exp e0)])
        ,(rco-exp e1))]
    [_
     ((current-R1-mismatch-handler) 'exp e)]))

(define (rco-arg e)
  (match e
    [(? fixnum?)
     (values e '())]
    [`(read)
     (define v ((current-gensym)))
     (values v `((,v . ,e)))]
    [`(- ,e0)
     (define-values (v0 env0) (rco-arg e0))
     (define v ((current-gensym)))
     (values v (append env0 `((,v . (- ,v0)))))]
    [`(+ ,e0 ,e1)
     (define-values (v0 env0) (rco-arg e0))
     (define-values (v1 env1) (rco-arg e1))
     (define v ((current-gensym)))
     (values v (append env0 env1 `((,v . (+ ,v0 ,v1)))))]
    [(? symbol?)
     (values e '())]
    [`(let ([,x ,e0]) ,e1)
     (define-values (v1 env1) (rco-arg e1))
     (values v1 (append `((,x . ,(rco-exp e0)))
                        env1))]
    [_
     ((current-R1-mismatch-handler) 'arg e)]))

;; Aux

(define current-R1-mismatch-handler
  (make-parameter
   (lambda (kind term)
     (raise-arguments-error 'remove-complex-opera*-pass-R1
                            "failed to match"
                            "kind" kind
                            "term" term))))

(define (add-lets env e)
  (let loop ([env env])
    (cond
      [(empty? env) e]
      [else
       (match-define (cons var val) (first env))
       `(let ([,var ,val])
          ,(loop (rest env)))])))

(define-syntax-rule (log message data)
  (let ([str-expr (format "who: ~a\n  ~a" message data)])
    (log-error str-expr)))
