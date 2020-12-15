#lang racket

(provide uniquify-tests)

(require rackunit
         "check-pass.rkt")

(require siek)

(define-test-suite uniquify-tests
  (check-pass* uniquify-pass-R1
               (R1 -> R1)
               2
               (let ([x 32])
                 (+ x 10))
               (let ([x (let ([x 4])
                          (+ x 1))])
                 (+ x 2))))
