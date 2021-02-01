#lang racket

(provide uncover-locals-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests uncover-locals-tests))

(define-compiler compiler
  (uniquify-R1
   normalize-R1
   explicate-control-R1
   remove-jumps-R1
   uncover-locals-R1))

(define-compiler-test-suite uncover-locals-tests
  #:compiler compiler
  #:signature (R1 -> C0)
  2
  (- 10)
  (- (+ 10 20))
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))
