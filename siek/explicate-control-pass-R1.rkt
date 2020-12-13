#lang racket

(provide explicate-control-pass-R1)

(require "raise-mismatch-error.rkt")

(define (explicate-control-pass-R1 p)
  (match p
    [`(program () ,e)
     `(program
       ()
       ((start . ,(explicate-tail e))))]
    [_
     (raise-mismatch-error 'explicate-control-pass-R1 'top p)]))

(define (explicate-tail e)
  (match e
    [(? fixnum?) `(return ,e)]
    [`(read) `(return ,e)]
    [`(- ,e0) `(return ,e)]
    [`(+ ,e0 ,e1) `(return ,e)]
    [(? symbol?) `(return ,e)]
    [`(let ([,x ,e0]) ,e1)
     (explicate-assign e0 x (explicate-tail e1))]
    [_
     (raise-mismatch-error 'explicate-control-pass-R1 'tail e)]))

(define (explicate-assign e v t)
  (match e
    [`(let ([,x ,e0]) ,e1)
     (explicate-assign
      e0
      x
      (explicate-assign e1 v t))]
    [(or
      (? fixnum?)
      (? symbol?)
      `(read)
      `(- ,_)
      `(+ ,_ ,_))
     `(seq (assign ,v ,e) ,t)]
    [_
     (raise-mismatch-error 'explicate-control-pass-R1 'assign e)]))
