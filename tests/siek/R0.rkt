#lang racket

(require rackunit)
(require siek)

(check-true (R0? '(program () 42)))
(check-true (R0? '(program () (- 10))))
(check-true (R0? '(program () (+ (read) (- 8)))))
(check-false (R0? '(program () (+ (read) (+ 8)))))
