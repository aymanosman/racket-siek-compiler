#lang racket

(provide explicate-control-R1%
         explicate-control-R1)

(require "raise-mismatch-error.rkt")

(define (explicate-control-R1 p)
  (send (new explicate-control-R1%) explicate p))

(define explicate-control-R1%
  (class object%
    (super-new)

    (define/public (who)
      'explicate-control-R1)

    (define/public (explicate p)
      (match p
        [`(program ,info ,e)
         `(program
           ,info
           ((start . ,(explicate-tail e))))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (explicate-tail e)
      (match e
        [(or (? atom?) (? prim?))
         `(return ,e)]
        [`(let ([,x ,e0]) ,e1)
         (explicate-assign e0 x (explicate-tail e1))]
        [_
         (raise-mismatch-error (who) 'tail e)]))

    (define/public (explicate-assign e v t)
      (match e
        [`(let ([,x ,e0]) ,e1)
         (explicate-assign e0 x (explicate-assign e1 v t))]
        [(or (? atom?) (? prim?))
         `(seq (assign ,v ,e) ,t)]
        [_
         (raise-mismatch-error (who) 'assign e)]))

    (define/public (prim? e)
      (match e
        [(or `(read ,a* ...)
             `(- ,a* ...)
             `(+ ,a* ...))
         #:when (andmap (lambda (a) (atom? a)) a*)
         #t]
        [_ #f]))

    (define/public (atom? a)
      (match a
        [(? fixnum?) #t]
        [(? symbol?) #t]
        [_ #f]))))
