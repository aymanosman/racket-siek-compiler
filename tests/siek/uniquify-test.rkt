#lang racket

(provide uniquify-R1-tests
         uniquify-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests uniquify-R1-tests)
  (run-tests uniquify-R2-tests))

(define-compiler-test-suite uniquify-R1-tests
  #:compiler uniquify-R1
  #:signature (R1 -> R1)
  2
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))

(define-extended-compiler-test-suite uniquify-R2-tests uniquify-R1-tests
  #:compiler uniquify-R2
  #:signature (R2 -> R2)
  (if (if (let ([x #t]) x) (eq? 1 2) (< 1 2))
      (let ([x 10])
        x)
      (let ([x 20])
        x)))
