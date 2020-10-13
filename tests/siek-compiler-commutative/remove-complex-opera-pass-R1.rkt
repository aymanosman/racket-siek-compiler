#lang racket

(require tests/siek-compiler-tester/check-pass)

(require siek-compiler-pass/uniquify-pass-R1
         siek-compiler-pass/remove-complex-opera-pass-R1)

(define compile (compose1 remove-complex-opera*-pass-R1
                          uniquify-pass-R1))

(check-pass* compile (R1 -> R1)
  2

  (let ([x 32])
    (+ x 10))

  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))
