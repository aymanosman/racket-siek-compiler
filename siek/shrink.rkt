#lang racket

(provide shrink-R2)

(require "options.rkt"
         "raise-mismatch-error.rkt")

(define (shrink-R2 p)
  (send (new shrink-R2%) shrink p))

(define shrink-R2%
  (class object%
    (super-new)

    (define/public (who)
      'shrink-R2)

    (define/public (shrink p)
      (match p
        [`(program ,info ,e)
         `(program ,info ,(shrink-exp e))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define-match-expander prim
      (lambda (stx)
        (syntax-case stx ()
          [(_ op)
           #'(list op)]
          [(_ op a)
           #'(list op (app shrink-exp a))]
          [(_ op a0 a1)
           #'(list op (app shrink-exp a0) (app shrink-exp a1))])))

    (define/public (shrink-exp e)
      (match e
        [(? atom?) e]
        [(prim '- e0 e1)
         `(+ ,e0 (- ,e1))]
        [(prim 'and e0 e1)
         `(if ,e0 ,e1 #f)]
        [(prim 'or e0 e1)
         `(if ,e0 #t ,e1)]
        [(prim '>= e0 e1)
         `(not (< ,e0 ,e1))]
        ;; TODO don't introduce bindings for atoms
        [(prim '<= e0 e1)
         (define tmp (fresh 'tmp))
         `(let ([,tmp ,e0])
            (not (< ,e1 ,tmp)))]
        [(prim '> e0 e1)
         (define tmp (fresh 'tmp))
         `(let ([,tmp ,e0])
            (< ,e1 ,tmp))]
        [`(let ([,x ,e0]) ,e1)
         `(let ([,x ,(shrink-exp e0)]) ,(shrink-exp e1))]
        [`(if ,e0 ,e1 ,e2)
         `(if ,(shrink-exp e0) ,(shrink-exp e1) ,(shrink-exp e2))]
        [(prim op)
         (list op)]
        [(prim op a)
         (list op a)]
        [(prim op a0 a1)
         (list op a0 a1)]
        [_
         (raise-mismatch-error (who) 'exp e)]))

    (define/public (atom? a)
      (or (fixnum? a)
          (boolean? a)
          (symbol? a)))))
