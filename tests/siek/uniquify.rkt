#lang racket

(provide uniquify-R1-tests
         uniquify-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

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
  (let ([x (not #f)])
    (and x #t)))