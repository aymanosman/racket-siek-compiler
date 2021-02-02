#lang racket

(provide normalize-R1-tests
         normalize-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests normalize-R1-tests)
  (run-tests normalize-R2-tests))

(define-compiler compiler-R1
  (uniquify-R1
   normalize-R1))

;; (compiler-trace! compiler-R1 #t)

(define-compiler-test-suite normalize-R1-tests
  #:compiler compiler-R1
  #:signature (R1 -> R1†)
  2
  (+ (+ 1 2) 2)
  (let ([x 32])
    (+ x 10))
  (let ([x 32])
    (let ([y (+ 10 x)])
      x))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))

(define-compiler compiler-R2
  (shrink-R2
   uniquify-R2
   normalize-R2))

;; (compiler-trace! compiler-R2 #t)

(define-extended-compiler-test-suite normalize-R2-tests normalize-R1-tests
  #:compiler compiler-R2
  #:signature (R2 -> R2†)
  (not (< 1 2))
  [(- (read) (read)) <= "13 3"]
  (+ (if (eq? 1 2) 1 2) 3)
  (if (not (eq? 1 2))
      10
      20)
  (if (if (< 1 2) #t #f)
      12
      13))
