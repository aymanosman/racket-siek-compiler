#lang racket

(provide C0%
         interp-C0)

(require racket/fixnum
         "raise-mismatch-error.rkt")

;; tail := (return e) |  (seq s t)
;; stmt := (assign x e)
;; exp  := a | (read) | (- a) | (+ a a)
;; atom := int | var

(define (interp-C0 p)
  (send (new C0%) interp p))

(define C0%
  (class object%
    (super-new)

    (define/public (who-interp)
      'interp-C0)

    (define/public (interp p)
      (match p
        [`(program ,_ ,code)
         (define tail (dict-ref code 'start))
         (interp-tail code '() tail)]
        [_
         (raise-mismatch-error (who-interp) 'top p)]))

    (define/public (interp-tail code env t)
      (match t
        [`(return ,e) (interp-exp env e)]
        [`(seq ,s ,t) (interp-tail code (interp-stmt env s) t)]
        [_
         (raise-mismatch-error (who-interp) 'tail t)]))

    (define/public (interp-stmt env s)
      (match s
        [`(assign ,v ,e) (dict-set env v (interp-exp env e))]
        [_
         (raise-mismatch-error (who-interp) 'stmt s)]))

    (define/public (interp-exp env e)
      (match e
        [(? symbol?) (dict-ref env e)]
        [(? fixnum?) e]
        [`(- ,a)
         #:when
         (atom? a)
         (fx- 0 (interp-exp env a))]
        [`(+ ,a0 ,a1)
         #:when
         (and (atom? a0) (atom? a1))
         (fx+ (interp-exp env a0) (interp-exp env a1))]
        [`(read)
         (match (read)
           [(? fixnum? r) r]
           [other
            (raise-argument-error (who-interp) "fixnum?" other)])]
        [_
         (raise-mismatch-error (who-interp) 'exp e)]))

    (define/public (atom? a)
      (match a
        [(? fixnum?) #t]
        [(? symbol?) #t]
        [_ #f]))))