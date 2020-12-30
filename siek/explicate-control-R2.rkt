#lang racket

(provide explicate-control-R2)

(require "options.rkt"
         "explicate-control-pass-R1.rkt"
         "raise-mismatch-error.rkt")

(define (explicate-control-R2 p)
  (send (new explicate-control-R2%) explicate p))

(define explicate-control-R2%
  (class explicate-control-R1%
    (super-new)

    (define/override (who)
      'explicate-control-R2)

    (define/override (explicate p)
      (match p
        [`(program ,info ,e)
         `(program
           ,info
           ,(explicate-tail 'start e))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/override (explicate-tail label e)
      (match e
        [(or (? atom?) (? prim?))
         (list (cons label `(return ,e)))]
        [`(let ([,x ,e0]) ,e1)
         (define l (fresh 'block))
         (append (explicate-assign label e0 x `(goto ,l))
                 (explicate-tail l e1))]
        [`(if ,e0 ,e1 ,e2)
         (define then (fresh 'block))
         (define else (fresh 'block))
         (append
          (explicate-pred label e0
                          `(goto ,then)
                          `(goto ,else))
          (explicate-tail then e1)
          (explicate-tail else e2))]
        [_
         (raise-mismatch-error (who) 'tail e)]))

    (define/override (explicate-assign label e v t)
      (match e
        [`(let ([,x ,e0]) ,e1)
         (define l (fresh 'block))
         (append (explicate-assign label e0 x `(goto ,l))
                 (explicate-assign l e1 v t))]
        [(or (? atom?) (? prim?))
         (list (cons label `(seq (assign ,v ,e) ,t)))]
        [`(if ,e0 ,e1 ,e2)
         (define body (fresh 'block))
         (define then (fresh 'block))
         (define else (fresh 'block))
         (append (explicate-pred label e0 `(goto ,then) `(goto ,else))
                 (explicate-assign then e1 v `(goto ,body))
                 (explicate-assign else e2 v `(goto ,body))
                 (list (cons body t)))]
        [_
         (raise-mismatch-error (who) 'assign e)]))

    (define/public (explicate-pred label e0 g0 g1)
      (match e0
        [#t (list (cons label g0))]
        [#f (list (cons label g1))]
        [(or (? atom?) (? prim?))
         (list (cons label
                     `(if ,e0
                          ,g0
                          ,g1)))]
        [`(let ([,x ,e1]) ,e2)
         (define l (fresh 'block))
         (append (explicate-assign label e1 x `(goto ,l))
                 (explicate-pred l e2 g0 g1))]
        [`(if ,e1 ,e2 ,e3)
         (define then2 (fresh 'block))
         (define else2 (fresh 'block))
         (append
          (explicate-pred label e1 `(goto ,then2) `(goto ,else2))
          (explicate-pred then2 e2 g0 g1)
          (explicate-pred else2 e3 g0 g1))]))

    (define/override (atom? a)
      (match a
        [(? boolean?) #t]
        [_ (super atom? a)]))

    (define/override (prim? e)
      (match e
        [(or `(eq? ,a* ...)
             `(< ,a* ...)
             ;; `(<= ,a* ...)
             ;; `(> ,a* ...)
             ;; `(>= ,a* ...)
             `(not ,a* ...)
             `(and ,a* ...)
             `(or ,a* ...))
         #:when (andmap (lambda (a) (atom? a)) a*)
         #t]
        [_
         (super prim? e)]))))
