#lang racket

(provide select-instructions-tests)

(require rackunit
         "define-compiler-test-suite.rkt")

(require siek)

(define compile
  (compose1 select-instructions-R1
            uncover-locals-R1
            explicate-control-R1
            normalize-R1
            uniquify-R1))

(define-compiler-test-suite select-instructions-tests
  #:compiler compile
  #:signature (R1 -> x860*)
  2
  (- 10)
  (- (+ 10 20))
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))
