#lang racket

(provide normalize-R2-tests)

(require rackunit
         siek
         "define-compiler-test-suite.rkt"
         "normalize-R1.rkt")

(define-compiler compile
  (uniquify-R2
   normalize-R2))

(define-compiler-test-suite normalize-R2-tests
  #:compiler compile
  #:signature (R2 -> R2) ;; R2†
  (not (< 1 2)))
