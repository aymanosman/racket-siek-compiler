#lang racket

(require rackunit)
(require siek)

(check-true (R1? '(program () 42)))
(check-true (R1? '(program () (- 10))))
(check-true (R1? '(program () (+ (read) (- 8)))))
(check-false (R1? '(program () (+ (read) (+ 8)))))
(check-true (R1? '(program ()
                    (let ([x (+ 12 20)])
                      (+ 10 x)))))
