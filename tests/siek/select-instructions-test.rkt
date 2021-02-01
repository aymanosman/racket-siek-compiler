#lang racket

(provide select-instructions-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests select-instructions-tests)
  (run-tests select-instructions-R2-tests))

(define-compiler compiler-R1
  (uniquify-R1
   normalize-R1
   explicate-control-R1
   remove-jumps-R1
   uncover-locals-R1
   select-instructions-R1))

(define-compiler-test-suite select-instructions-tests
  #:compiler compiler-R1
  #:signature (R1 -> x860*)
  2
  (- 10)
  (- (+ 10 20))
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))

(define-compiler compiler-R2
  (typecheck-R2
   shrink-R2
   uniquify-R2
   normalize-R2
   explicate-control-R2
   remove-jumps-R2
   uncover-locals-R2
   select-instructions-R2))

(compiler-trace! compiler-R2 #t)

(define-extended-compiler-test-suite select-instructions-R2-tests select-instructions-tests
  #:compiler compiler-R2
  #:signature (R2 -> x861*)
  (if (eq? 1 2)
      10
      20)
  (if (< 1 2)
      10
      20)
  (if (or #f #t)
      10
      20)
  (if (<= 1 2)
      10
      20)
  (if (>= 1 2)
      10
      20))
