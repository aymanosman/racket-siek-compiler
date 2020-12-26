#lang racket

(provide R2%
         R2†%
         interp-R2
         interp-R2†
         cmp?)

(require "R1.rkt"
         "raise-mismatch-error.rkt")

;; bool := #t | #f
;; cmp := eq? | < | <= | > | >=
;; exp := ...
;;      | (- e e)
;;      | bool | (and e e) | (or e e) | (not e)
;;      | (cmp e e) | (if e e e)

(define (interp-R2 p)
  (send (new R2%) interp p))

(define R2%
  (class R1%
    (super-new)

    (define/override (who)
      'interp-R2)

    (define/override ((interp-exp env) e)
      (match e
        [`(- ,e0 ,e1)
         (- ((interp-exp env) e0) ((interp-exp env) e1))]
        [(? boolean?) e]
        [`(if ,cond ,then ,else)
         (match ((interp-exp env) cond)
           [#t ((interp-exp env) then)]
           [#f ((interp-exp env) else)]
           [_
            (raise-mismatch-error (who) 'exp cond)])]
        [`(and ,e0 ,e1)
         (match ((interp-exp env) e0)
           [#t ((interp-exp env) e1)]
           [#f #f])]
        [`(or ,e0 ,e1)
         (match ((interp-exp env) e0)
           [#f ((interp-exp env) e1)]
           [#t #t])]
        [`(,op ,e* ...)
         #:when
         (op? op)
         (interp-op op (map (interp-exp env) e*))]
        [_
         ((super interp-exp env) e)]))

    (define/public (op? v)
      (member v '(eq? < <= > >= and or not - read +)))

    (define/public (operator-implementation)
      (hash 'eq?
            eq?
            '<
            <
            '<=
            <=
            '>
            >
            '>=
            >=
            'and
            and-fn
            'or
            or-fn
            'not
            not
            '-
            -
            'read
            read
            '+
            +))

    (define/public (interp-op op args)
      (apply (hash-ref (operator-implementation) op) args))))

(define (and-fn a b)
  (and a b))

(define (or-fn a b)
  (or a b))

(define (cmp? v)
  (member v '(eq? < <= > >=)))

;; exp := ...
;;      | (not a)
;;      | (cmp a a)
;;      | (if e e e)
;; atom := ... | bool

(define (interp-R2† p)
  (send (new R2†%) interp p))

(define R2†%
  (class R1†%
    (super-new)

    (define/override (who)
      'interp-R2†)

    (define/override ((interp-exp env) e)
      (match e
        [`(not ,a)
         (not ((interp-atom env) a))]
        [`(,c ,a0 ,a1)
         #:when (cmp? c)
         (define op
           (case c
             [(eq?) =]
             [(<) <]
             [else (error (who) "todo op")]))
         (op ((interp-atom env) a0)
             ((interp-atom env) a1))]
        [`(if ,e0 ,e1 ,e2)
         (match ((interp-exp env) e0)
           [#t ((interp-exp env) e1)]
           [#f ((interp-exp env) e2)])]
        [_
         ((super interp-exp env) e)]))

    (define/override ((interp-atom env) a)
      (match a
        [(? boolean?) a]
        [_
         ((super interp-atom env) a)]))

    (define/override (atom? a)
      (match a
        [(? boolean?) #t]
        [_
         (super atom? a)]))))
