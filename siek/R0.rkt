#lang racket

(provide R0%
         R0?
         interp-R0)

(require "raise-mismatch-error.rkt")

;; exp := int | (read) | (- e) | (+ e e)

(define (R0? p)
  (send (new R0%) ? p))

(define (interp-R0 p)
  (send (new R0%) interp p))

(define R0%
  (class object%
    (super-new)

    (define/public (who-interp)
      'interp-R0)

    (define/public (? v)
      (match v
        [`(program () ,e) (exp? e)]
        [_ #f]))

    (define/public (exp? v)
      (match v
        [(? fixnum?) #t]
        [`(read) #t]
        [`(- ,e) (exp? e)]
        [`(+ ,e0 ,e1) (and (exp? e0) (exp? e1))]
        [_ #f]))

    (define/public (interp p)
      (match p
        [`(program ,_ ,e)
         ((interp-exp '()) e)]
        [_
         (raise-mismatch-error (who-interp) 'top p)]))

    (define/public ((interp-exp env) v)
      (match v
        [(? fixnum? n) n]
        [`(read)
         (match (read)
           [(? fixnum? n) n]
           [other
            (raise-argument-error (who-interp) "fixnum?" other)])]
        [`(- ,e)
         (- ((interp-exp env) e))]
        [`(+ ,e0 ,e1)
         (+ ((interp-exp env) e0) ((interp-exp env) e1))]
        [_
         (raise-mismatch-error (who-interp) 'exp v)]))))