#lang racket

(provide typecheck-R0%
         typecheck-R0)

(require racket/control
         "options.rkt")

(define (typecheck-R0 p)
  (send (new typecheck-R0%) typecheck p))

(define typecheck-R0%
  (class object%
    (super-new)

    (define/public (who)
      'typecheck-R0)

    (define/public (typecheck p)
      (match p
        [`(program ,info ,e)
         (define-values (errors type)
           (collect-type-errors
            ((typecheck-exp '()) e)))
         (define new-info
           (cond
             [(empty? errors)
              (dict-set info 'type type)]
             [else
              (dict-set info 'type-errors errors)]))
         `(program ,new-info ,e)]
        [_
         (raise-mismatch-error (who) 'top p)]))

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
         (raise-mismatch-error (who) 'exp e)]))

    (define/public (type=? t1 t2)
      (equal? t1 t2))

    (define/public (fail-typecheck who . args)
      (define err (current-type-errors))
      (cond
        [(box? err)
         (set-box! err (append (unbox err) args))]
        [else
         (apply raise-arguments-error
                (who)
                "called outside of (collect-type-errors) context"
                args)]))

    (define/public (expect-type=? t1 t2)
      (unless (type=? t1 t2)
        (fail-typecheck (who) "expected" t2 "given" t1)
        (abort #f))
      t1)

    (define/public (expect-type env e t)
      (expect-type=? ((typecheck-exp env) e) t))

    (define-syntax-rule (collect-type-errors body)
      (parameterize ([current-type-errors (box '())])
        (let ([t (prompt body)])
          (values (unbox (current-type-errors)) t))))

    (define/public (report-type-errors errors)
      (apply raise-arguments-error
             (who)
             "type mismatch"
             errors))))
