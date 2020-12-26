#lang racket

(provide R0%
         R0†%
         interp-R0
         interp-R0†)

(require "raise-mismatch-error.rkt")

;; exp := int | (read) | (- e) | (+ e e)

(define (interp-R0 p)
  (send (new R0%) interp p))

(define R0%
  (class object%
    (super-new)

    (define/public (who)
      'interp-R0)

    (define/public (interp p)
      (match p
        [`(program ,_ ,e)
         ((interp-exp '()) e)]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public ((interp-exp env) v)
      (match v
        [(? fixnum? n) n]
        [`(read)
         (interp-read)]
        [`(- ,e)
         (- ((interp-exp env) e))]
        [`(+ ,e0 ,e1)
         (+ ((interp-exp env) e0) ((interp-exp env) e1))]
        [_
         (raise-mismatch-error (who) 'exp v)]))

    (define/public (interp-read)
      (match (read)
        [(? fixnum? n) n]
        [other
         (raise-argument-error (who) "fixnum?" other)]))))

;; exp := a | (read)
;;      | (- a) | (+ a a)
;; atom := n

(define (interp-R0† p)
  (send (new R0†%) interp p))

(define R0†%
  (class R0%
    (super-new)

    (inherit interp-read)

    (define/override (who)
      'interp-R0†)

    (define/override (interp p)
      (match p
        [`(program ,_ ,e)
         ((interp-exp '()) e)]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/override ((interp-exp env) e)
      (match e
        [(? atom? a)
         a]
        [`(read)
         (interp-read)]
        [`(- ,a)
         (- ((interp-atom env) a))]
        [`(+ ,a0 ,a1)
         (+ ((interp-atom env) a0) ((interp-atom env) a1))]
        [_
         (raise-mismatch-error (who) 'exp e)]))

    (define/public ((interp-atom env) a)
      (match a
        [(? fixnum?) a]
        [_ (raise-mismatch-error (who) 'atom a)]))

    (define/public (atom? a)
      (match a
        [(? fixnum?) #t]
        [_ #f]))))
