#lang racket

(provide typecheck-R1%
         typecheck-R1)

(require "typecheck-R0.rkt")

(define (typecheck-R1 p)
  (send (new typecheck-R1%) typecheck p))

(define typecheck-R1%
  (class typecheck-R0%
    (super-new)

    (inherit expect-type)

    (define/override (who)
      'typecheck-R1)

    (define/override ((typecheck-exp env) e)
      (match e
        [(? symbol? x)
         (dict-ref env x)]
        [`(let ([,x ,e0]) ,e1)
         ((typecheck-exp (dict-set env x (expect-type env e0 'Integer)))
          e1)]
        [_
         ((super typecheck-exp env) e)]))))
