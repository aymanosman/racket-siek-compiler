#lang racket

(provide R0%
         R0?
         interp-R0
         typecheck-R0)

(require racket/control
         "options.rkt"
         "raise-mismatch-error.rkt")

;; exp := int | (read) | (- e) | (+ e e)

(define (R0? p)
  (send (new R0%) ? p))

(define (interp-R0 p)
  (send (new R0%) interp p))

(define (typecheck-R0 p)
  (send (new R0%) typecheck p))

(define R0%
  (class object%
    (super-new)

    (define/public (who-interp)
      'interp-R0)

    (define/public (who-typecheck)
      'typecheck-R0)

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
      (typecheck p)
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
         (raise-mismatch-error (who-interp) 'exp v)]))

    (define-syntax-rule (collect-type-errors body)
      (parameterize ([current-type-errors (box '())])
        (let ([t (prompt body)])
          (values (unbox (current-type-errors)) t))))

    (define/public (report-type-errors errors)
      (apply raise-arguments-error
             (who-typecheck)
             "type mismatch"
             errors))

    (define/public (typecheck p)
      (define-values (errors t) (typecheck/collect p))
      (when (not (empty? errors))
        (report-type-errors errors))
      t)

    (define/public (typecheck/collect p)
      (match p
        [`(program ,_ ,e)
         (collect-type-errors
          ((typecheck-exp '()) e))]
        [_
         (raise-mismatch-error (who-typecheck) 'top p)]))

    (define/public ((typecheck-exp env) e)
      (match e
        [(? fixnum? n) 'Integer]
        [`(read) 'Integer]
        [`(- ,e)
         (expect-type env e 'Integer)]
        [`(+ ,e0 ,e1)
         (expect-type env e0 'Integer)
         (expect-type env e1 'Integer)]
        [_
         (raise-mismatch-error (who-typecheck) 'exp e)]))

    (define/public (type=? t1 t2)
      (equal? t1 t2))

    (define/public (fail-typecheck who . args)
      (define err (current-type-errors))
      (cond
        [(box? err)
         (set-box! err (append (unbox err) args))]
        [else
         (apply raise-arguments-error
                (who-typecheck)
                "called outside of (collect-type-errors) context"
                args)]))

    (define/public (expect-type=? t1 t2)
      (unless (type=? t1 t2)
        (fail-typecheck (who-typecheck) "expected" t2 "given" t1 )
        (abort #f))
      t1)

    (define/public (expect-type env e t)
      (expect-type=? ((typecheck-exp env) e) t))))
