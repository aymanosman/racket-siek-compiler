#lang racket

(provide allocate-registers-tests)

(require rackunit
         "check-pass.rkt")

(require siek)

(module+ test
  (require rackunit/text-ui)
  (run-tests allocate-registers-tests))

(define-compiler compile
                 (uniquify-R1
                  normalize-R1
                  explicate-control-pass-R1
                  uncover-locals-pass-R1
                  select-instructions-pass-R1
                  uncover-live-pass-R1
                  build-interference-pass-R1
                  assign-colors-R1
                  allocate-registers-pass-R1
                  patch-instructions-pass-R1))

;; (compiler-trace! compile #t)

(define-test-suite allocate-registers-tests
  (test-compiler compile
                 (R1 -> x860)
                 2
                 (- 10)
                 (- (+ 10 20))
                 (let ([x 32])
                   (+ x 10))
                 (let ([x (let ([x 4])
                            (+ x 1))])
                   (+ x 2))))
