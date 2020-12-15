#lang racket

(provide interp-R0)

(require racket/fixnum
         "raise-mismatch-error.rkt")

(define (interp-R0 p)
  (match p
    [`(program () ,e)
     (interp-exp e)]
    [_
     (raise-mismatch-error 'interp-R0 'top p)]))

(define (interp-exp e)
  (match e
    [(? fixnum?) e]
    [`(read)
     (match (read)
       [(? fixnum? n) n]
       [other
        (raise-argument-error 'interp-R0 "fixnum?" other)])]
    [`(- ,e) (fx- 0 (interp-exp e))]
    [`(+ ,e0 ,e1) (fx+ (interp-exp e0) (interp-exp e1))]
    [_
     (raise-mismatch-error 'interp-R0 'exp e)]))
