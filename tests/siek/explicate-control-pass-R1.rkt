#lang racket

(provide explicate-control-tests)

(require rackunit
         "check-pass.rkt")

(require siek)

(define compile
  (compose1 explicate-control-pass-R1
            remove-complex-opera*-pass-R1
            uniquify-pass-R1))

(define-test-suite explicate-control-tests
  (check-pass* compile
               (R1 -> C0)
               2
               (let ([x 32])
                 (+ x 10))
               (let ([x (let ([x 4])
                          (+ x 1))])
                 (+ x 2))))
