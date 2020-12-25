#lang racket

(provide uniquify-R2)

(require "gensym.rkt"
         "uniquify-pass-R1.rkt")

;; bool := #t | #f
;; cmp := eq? | < | <= | > | >=
;; exp := ...
;;      | (- e e)
;;      | bool | (and e e) | (or e e) | (not e)
;;      | (cmp e e) | (if e e e)

(define (uniquify-R2 p)
  (send (new uniquify-R2%) uniquify p))

(define uniquify-R2%
  (class uniquify-R1%
    (super-new)

    (define (who)
      'uniquify-R2)

    (define/override ((uniquify-exp env) e)
      (define recur (uniquify-exp env))
      (match e
        [(? boolean?)
         e]
        [`(- ,e0 ,e1)
         `(- ,(recur e0) ,(recur e1))]
        [`(and ,e0 ,e1)
         `(and ,(recur e0) ,(recur e1))]
        [`(or ,e0 ,e1)
         `(or ,(recur e0) ,(recur e1))]
        [`(not ,e)
         `(not ,(recur e))]
        [`(,c ,e0 ,e1)
         #:when
         (cmp? c)
         `(,c ,(recur e0) ,(recur e1))]
        [`(if ,e0 ,e1 ,e2)
         `(if ,(recur e0) ,(recur e1) ,(recur e2))]
        [_
         ((super uniquify-exp env) e)]))))

(define (cmp? v)
  (member v '(eq? < <= > >=)))