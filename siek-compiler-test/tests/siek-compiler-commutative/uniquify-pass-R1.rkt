#lang racket

(require tests/siek-compiler-tester/check-pass)

(require siek-compiler-pass/uniquify-pass-R1)

(check-pass* uniquify-pass-R1 (R1 -> R1)
            2

            (let ([x 32])
              (+ x 10))

            (let ([x (let ([x 4])
                       (+ x 1))])
              (+ x 2)))
