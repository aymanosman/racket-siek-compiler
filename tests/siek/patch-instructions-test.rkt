#lang racket

(provide patch-instructions-tests
         patch-instructions-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests patch-instructions-tests)
  (run-tests patch-instructions-R2-tests))

(define-compiler compiler-R1
  (uniquify-R1
   normalize-R1
   explicate-control-R1
   remove-jumps-R1
   uncover-locals-R1
   select-instructions-R1
   assign-homes-R1
   patch-instructions-R1))

;; (compiler-trace! compiler-R1 #t)

(define-compiler-test-suite patch-instructions-tests
  #:compiler compiler-R1
  #:signature (R1 -> x860)
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
   select-instructions-R2
   assign-homes-R2
   patch-instructions-R2))

;; (compiler-trace! compiler-R2 #t)

(define-extended-compiler-test-suite patch-instructions-R2-tests patch-instructions-tests
  #:compiler compiler-R2
  #:signature (R2 -> x861)
  (if (not #f)
      10
      20)
  (if (< 1 2)
      10
      20)
  (if (<= 1 2)
      10
      20)
  (if (>= 1 2)
      10
      20)
  (if (and #t #t)
      10
      20)
  (if (or #f #t)
      10
      20))
