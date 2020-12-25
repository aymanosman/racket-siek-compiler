#lang racket

(provide normalize-R1-tests
         normalize-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

(define-compiler compile-R1
  (uniquify-R1
   normalize-R1))

(define-compiler-test-suite normalize-R1-tests
  #:compiler compile-R1
  #:signature (R1 -> R1)
  2
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))

(define-compiler compile-R2
  (uniquify-R2
   normalize-R2))

(define-extended-compiler-test-suite normalize-R2-tests normalize-R1-tests
  #:compiler compile-R2
  #:signature (R2 -> R2) ;; R2†
  (not (< 1 2)))
