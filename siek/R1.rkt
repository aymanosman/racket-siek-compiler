#lang racket

(provide R1%
         interp-R1)

(require "R0.rkt")

;; exp := ...
;;      | var | (let ([x e]) e)

(define (interp-R1 p)
  (send (new R1%) interp p))

(define R1%
  (class R0%
    (super-new)



    (define/override (who-interp)
      'interp-R1)

    (define/override ((interp-exp env) e)
      (match e
        [(? symbol? x)
         (dict-ref env x)]
        [`(let ([,x ,e0]) ,e1)
         ((interp-exp (dict-set env x ((interp-exp env) e0)))
          e1)]
        [_
         ((super interp-exp env) e)]))

   ))
