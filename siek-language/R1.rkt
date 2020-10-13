#lang racket

(provide R1?)

;; Exp := Int | (read) | (- Exp) | (+ Exp Exp)
;;      | Var | (let ([Var Exp]) Exp)
;; R1  := (program Info Exp)

(define (R1? p)
  (match p
    [`(program ,info ,e)
     (and (R1-info? info)
          (R1-exp? e))]
    [_ #f]))

(define (R1-info? i)
  (list? i))

(define (R1-exp? e)
  (match e
    [(? fixnum?) #t]
    [`(read) #t]
    [`(- ,e) (R1-exp? e)]
    [`(+ ,e0 ,e1) (and (R1-exp? e0) (R1-exp? e1))]
    [(? symbol?) #t]
    [`(let ([,var ,e0]) ,e1)
     (and (symbol? var) (R1-exp? e0) (R1-exp? e1))]
    [_ #f]))
