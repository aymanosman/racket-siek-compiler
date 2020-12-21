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
        [`(,op ,e0 ,e1)
         (app? op (list e0 e1))]
        [_ (super exp? e)]))

    (define/public (app? op e*)
      (and (op? op)
           (andmap (lambda (e) (exp? e) e*))))

    (define/public (op? op)
      (or (cmp? op)
          (member op '(read + - and or not))))

    (define/public (cmp? v)
      (member v '(eq? < <= > >=)))))