#lang racket

(require tests/siek-compiler-tester/check-pass)

(require siek-compiler-pass/uniquify-pass-R1
         siek-compiler-pass/remove-complex-opera-pass-R1
         siek-compiler-pass/explicate-control-pass-R1)

(define compile (compose1 explicate-control-pass-R1
                          remove-complex-opera*-pass-R1
                          uniquify-pass-R1))

(check-pass* compile (R1 -> C0)
  2

  (let ([x 32])
    (+ x 10))

  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))