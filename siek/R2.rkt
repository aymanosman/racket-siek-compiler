#lang racket

(provide R2%
         R2?
         interp-R2)

(require "R1.rkt")

;; bool := #t | #f
;; cmp := eq? | < | <= | > | >=
;; exp := ...
;;      | (- e e)
;;      | bool | (and e e) | (or e e) | (not e)
;;      | (cmp e e) | (if e e e)

(define (R2? v)
  (send (new R2%) ? v))

(define (interp-R2 p)
  (send (new R2%) interp p))

(define R2%
  (class R1%
    (super-new)

    (define/override (exp? e)
      (match e
        [`(- ,e0 ,e1)
         (and (exp? e0) (exp? e1))]
        [(? boolean?) #t]
        [`(if ,e0 ,e1 ,e2)
         (and (exp? e0) (exp? e1) (exp? e2))]
        [`(,op ,e* ...) #:when (op? op)
                        (andmap (lambda (e) (exp? e)) e*)]
        [_ (super exp? e)]))

    (define/override ((interp-exp env) e)
      (match e
        [`(- ,e0 ,e1)
         (- ((interp-exp env) e0) ((interp-exp env) e1))]
        [(? boolean?) e]
        [`(if ,cond ,then ,else)
         (match ((interp-exp env) cond)
           [#t ((interp-exp env) then)]
           [#f ((interp-exp env) else)])]
        [`(and ,e0 ,e1)
         (match ((interp-exp env) e0)
           [#t ((interp-exp env) e1)]
           [#f #f])]
        [`(or ,e0 ,e1)
         (match ((interp-exp env) e0)
           [#f ((interp-exp env) e1)]
           [#t #t])]
        [`(,op ,e* ...) #:when (op? op)
                        (interp-op op (map (interp-exp env) e*))]
        [_
         ((super interp-exp env) e)]))

    (define/public (op? v)
      (member v '(eq? < <= > >= and or not)))

    (define/public (operator-types)
      (hash 'eq? eq? '< < '<= <= '> > '>= >=))

    (define/public (interp-op op args)
      (apply (hash-ref (operator-types) op) args))))
