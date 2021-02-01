#lang racket

(provide uniquify-R1
         uniquify-R2)

(require (for-syntax "catamorphism.rkt")
         "gensym.rkt"
         "raise-mismatch-error.rkt")

(define (uniquify-R1 p)
  (send (new uniquify-R1%) uniquify p))

(define uniquify-R1%
  (class object%
    (super-new)

    (define/public (who)
      'uniquify-R1)

    (define/public (uniquify p)
      (match p
        [`(program ,info ,e)
         `(program ,info ,(uniquify-exp '() e))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (uniquify-exp env e)

      (define-match-expander prim
        (lambda (stx)
          (prim-cata #'(lambda (e) (uniquify-exp env e)) stx)))

      (match e
        ;; R0
        [(? fixnum?)
         e]
        [`(read)
         e]
        [(prim '- e)
         `(- ,e)]
        [(prim '+ e0 e1)
         `(+ ,e0 ,e1)]

        ;; R1
        [(? symbol?)
         (dict-ref env e #f)]
        [`(let ([,x ,e]) ,body)
          (define x.1 (fresh x))
         `(let ([,x.1 ,(uniquify-exp env e)])
            ,(uniquify-exp (dict-set env x x.1) body))]
        [_
         (raise-mismatch-error (who) 'exp e)]))))

(define (uniquify-R2 p)
  (send (new uniquify-R2%) uniquify p))

(define uniquify-R2%
  (class uniquify-R1%
    (super-new)

    (define/override (who)
      'uniquify-R2)

    (define/override (uniquify-exp env e)

      (define-match-expander prim
        (lambda (stx)
          (prim-cata #'(lambda (e) (uniquify-exp env e)) stx)))

      (define-match-expander if
        (lambda (stx)
          (syntax-case stx ()
            [(_ e* ...)
             #'(list 'if (app (lambda (e) (uniquify-exp env e)) e*) ...)])))

      (match e
        [(? boolean?)
         e]
        [(prim (and op (or 'not)) e)
         `(,op ,e)]
        [(prim (and op (or '- 'and 'or 'eq? '<)) e0 e1)
         `(,op ,e0 ,e1)]
        [(if e0 e1 e2)
         `(if ,e0 ,e1 ,e2)]
        [_
         (super uniquify-exp env e)]))))
