#lang racket

(provide R1%
         R1?
         interp-R1)

(require "R0.rkt")

;; exp := ...
;;      | var | (let ([x e]) e)

(define (R1? v)
  (send (new R1%) ? v))

(define (interp-R1 p)
  (send (new R1%) interp p))

(define R1%
  (class R0%
    (super-new)

    (define/override (who-interp)
      'interp-R1)

    (define/override (exp? v)
      (match v
        [(? symbol?) #t]
        [`(let ([,var ,e0]) ,e1)
         (and (symbol? var) (exp? e0) (exp? e1))]
        [_ (super exp? v)]))

    (define/override ((interp-exp env) e)
      (match e
        [(? symbol? x)
         (dict-ref env x)]
        [`(let ([,x ,e0]) ,e1)
         ((interp-exp (dict-set env x ((interp-exp env) e0)))
          e1)]
        [_
         ((super interp-exp env) e)]))))


;; TODO

;; R1† (ANF)

;; exp := ...
;;      | atom | (- a) | (+ a a)
;; atom := int | var


