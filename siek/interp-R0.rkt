#lang racket

(provide interp-R0)

(require racket/fixnum)

(define (interp-R0 p)
  (match p
    [`(program () ,e)
     (interp-exp e)]
     [_
       (raise-arguments-error 'interp-R0 "failed match"
                              "kind" 'top
                              "term" p)]))

(define (interp-exp e)
  (match e
    [(? fixnum?) e]
    [`(read)
      (match (read)
        [(? fixnum? n) n]
        [other
          (raise-argument-error 'interp-R0 "integer?" other)])]
    [`(- ,e) (fx- 0 (interp-exp e))]
    [`(+ ,e0 ,e1) (fx+ (interp-exp e0) (interp-exp e1))]
    [_
      (raise-arguments-error 'interp-R0 "failed match"
                             "kind" 'exp
                             "term" e)]))