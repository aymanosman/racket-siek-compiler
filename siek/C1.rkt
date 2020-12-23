#lang racket

(provide C1%
         interp-C1)

(require "C0.rkt")

;; tail := ...
;;       | (goto l)
;;       | (if (c a a) (goto l) (goto l))
;; stmt := ...
;; exp  := ...
;;       | (not a) | (c a a)
;; cmp  := eq? | <
;; atom := ...
;;       | bool

(define (interp-C1 p)
  (send (new C1%) interp p))

(define C1%
  (class C0%
    (super-new)

    (define/override (who-interp)
      'interp-C1)

    (define/override (interp-tail code env t)
      (match t
        [`(goto ,l)
         (interp-tail code env (dict-ref code l))]
        [`(if (,c ,a0 ,a1) (goto ,then) (goto ,else))
         #:when
         (and (cmp? c) (atom? a0) (atom? a1))
         (match (interp-cmp c a0 a1)
           [#t (interp-tail code env (dict-ref code then))]
           [#f (interp-tail code env (dict-ref code else))])]
        [_ (super interp-tail code env t)]))

    (define/override (interp-exp env e)
      (match e
        [`(not ,a)
         #:when
         (boolean? a)
         (match a
           [#t #f]
           [#f #t])]
        [`(,c ,a0 ,a1)
         #:when
         (and (cmp? c) (atom? a0) (atom? a1))
         (interp-cmp c a0 a1)]
        [_ (super interp-exp env e)]))

    (define/public (cmp? c)
      (and (member c '(eq? < <= > >=))))

    (define/public (interp-cmp c x y)
      (case c
        [(eq?) (= x y)]
        [(<) (< x y)]
        [(<=) (<= x y)]
        [(>) (> x y)]
        [(>=) (>= x y)]))

    (define/override (atom? a)
      (match a
        [(? boolean?) #t]
        [_ (super atom? a)]))))
