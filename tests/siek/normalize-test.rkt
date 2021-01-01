#lang racket

(provide normalize-R1-tests
         normalize-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests normalize-R1-tests)
  (run-tests normalize-R2-tests))

(define-compiler compile-R1
  (uniquify-R1
   normalize-R1))

(define-compiler-test-suite normalize-R1-tests
  #:compiler compile-R1
  #:signature (R1 -> R1†)
  2
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))

(define-compiler compile-R2
  (uniquify-R2
   normalize-R2))

;; (compiler-trace! compile-R2 #t)

(define-extended-compiler-test-suite normalize-R2-tests normalize-R1-tests
  #:compiler compile-R2
  #:signature (R2 -> R2†)
  (not (< 1 2))
  [(- (read) (read)) <= "13 3"]
  (if (if (< 1 2) #t #f)
      12
      13))
