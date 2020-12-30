#lang racket

(provide normalize-R1%
         normalize-R1)

(require "gensym.rkt"
         "raise-mismatch-error.rkt")

(define (normalize-R1 p)
  (send (new normalize-R1%) normalize p))

(define normalize-R1%
  (class object%
    (super-new)

    (define/public (who)
      'normalize-R1)

    (define/public (normalize p)
      (match p
        [`(program ,info ,e)
         `(program ,info ,(normalize-exp e))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (normalize-exp e)
      (match e
        [(? fixnum?) e]
        [`(read) e]
        [`(- ,e)
         (normalize-op '- e)]
        [`(+ ,e0 ,e1)
         (normalize-op '+ e0 e1)]
        [(? symbol?) e]
        [`(let ([,x ,e0]) ,e1)
         `(let
              ([,x ,(normalize-exp e0)])
            ,(normalize-exp e1))]
        [_
         (raise-mismatch-error (who) 'exp e)]))

    (define/public normalize-op
      (case-lambda
        [(op e)
         (define-values (x bindings) (normalize-arg e))
         (apply-bindings bindings `(,op ,x))]
        [(op e0 e1)
         (define-values (x0 bindings0) (normalize-arg e0))
         (define-values (x1 bindings1) (normalize-arg e1))
         (apply-bindings (append bindings0 bindings1) `(,op ,x0 ,x1))]))

    (define/public (normalize-arg e)
      (match e
        [(? fixnum?)
         (values e '())]
        [`(read)
         (arg-op 'read)]
        [`(- ,e)
         (arg-op '- e)]
        [`(+ ,e0 ,e1)
         (arg-op '+ e0 e1)]
        [(? symbol?)
         (values e '())]
        [`(let ([,x ,e0]) ,e1)
         (define-values (v1 env1) (normalize-arg e1))
         (values v1
                 (append `((,x . ,(normalize-exp e0)))
                         env1))]
        [_
         (raise-mismatch-error (who) 'arg e)]))

    (define/public arg-op
      (case-lambda
        [(op)
         (define tmp (fresh))
         (values tmp `((,tmp . (,op))))]
        [(op e)
         (define-values (x env) (normalize-arg e))
         (define tmp (fresh))
         (values tmp (append env `((,tmp . (,op ,x)))))]
        [(op e0 e1)
         (define-values (x0 env0) (normalize-arg e0))
         (define-values (x1 env1) (normalize-arg e1))
         (define x2 (fresh))
         (values x2 (append env0 env1 `((,x2 . (,op ,x0 ,x1)))))]))

    (define/public (apply-bindings bindings e)
      (cond
        [(empty? bindings) e]
        [else
         (match-define (cons var val) (first bindings))
         `(let
              ([,var ,val])
            ,(apply-bindings (rest bindings) e))]))))
