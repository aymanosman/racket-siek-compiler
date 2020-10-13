#lang racket

(provide R0?)

(define (R0? p)
  (match p
    [`(program () ,e) (R0-exp? e)]
    [_ #f]))

(define (R0-exp? e)
  (match e
    [(? fixnum?) #t]
    [`(read) #t]
    [`(- ,e) (R0-exp? e)]
    [`(+ ,e0 ,e1) (and (R0-exp? e0) (R0-exp? e1))]
    [_ #f]))