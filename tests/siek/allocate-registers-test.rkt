#lang racket

(provide allocate-registers-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests allocate-registers-tests))

(define-compiler compile
                 (uniquify-R1
                  normalize-R1
                  explicate-control-R1
                  uncover-locals-R1
                  select-instructions-R1
                  uncover-live-R1
                  build-interference-R1
                  assign-colors-R1
                  allocate-registers-R1
                  patch-instructions-R1))

;; (compiler-trace! compile #t)

(define-compiler-test-suite allocate-registers-tests
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
