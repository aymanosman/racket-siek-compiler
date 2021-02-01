#lang racket

(provide allocate-registers-tests
         allocate-registers-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests allocate-registers-tests)
  (run-tests allocate-registers-R2-tests))

(define-compiler compiler-R1
  (uniquify-R1
   normalize-R1
   explicate-control-R1
   remove-jumps-R1
   uncover-locals-R1
   select-instructions-R1
   uncover-live-R1
   uncover-conflicts-R1
   assign-colors-R1
   allocate-registers-R1
   patch-instructions-R1))

;; (compiler-trace! compile #t)

(define-compiler-test-suite allocate-registers-tests
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
   uncover-live-R2
   uncover-conflicts-R2
   assign-colors-R2
   allocate-registers-R2
   patch-instructions-R2))

;; (compiler-trace! compile #t)

(define-extended-compiler-test-suite allocate-registers-R2-tests allocate-registers-tests
  #:compiler compiler-R2
  #:signature (R1 -> x860)
  2
  (- 10)
  (- (+ 10 20))
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))
