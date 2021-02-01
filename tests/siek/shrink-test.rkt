#lang racket

(provide shrink-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests shrink-R2-tests))

(define-compiler compiler-R2
  (shrink-R2))

(define-compiler-test-suite shrink-R2-tests
  #:compiler compiler-R2
  #:signature (R2 -> R2) ;; TODO R2-shrunk
  [(> (read) (read)) <= "1 2"]
  [(>= (read) (read)) <= "1 2"]
  [(<= (read) (read)) <= "1 2"]
  [(> (read) (read)) <= "1 2"])
