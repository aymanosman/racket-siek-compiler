#lang racket

(provide assign-homes-tests)

(require rackunit
         "define-compiler-test-suite.rkt")

(require siek)

(define compile
  (compose1 assign-homes-pass-R1
            select-instructions-pass-R1
            uncover-locals-pass-R1
            explicate-control-pass-R1
            normalize-R1
            uniquify-R1))

(define-compiler-test-suite assign-homes-tests
  #:compiler compile
  #:signature (R1 -> x860)
  2
  (- 10)
  (- (+ 10 20))
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))
