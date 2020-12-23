#lang racket

(provide x860-tests)

(require rackunit
         siek)

(define-test-suite x860-tests
  (test-suite
   "x86"
   (test-case "valid x86"
     (check-pred
      x860?
      '(program
        ()
        ((start .
                (block ()
                       (movq (int 10) (reg rax))))))))
   (test-case "valid x86"
     (check-pred
      (compose not x860?)
      '(program
        ()
        ((start .
                (block ()
                       (movq (int 10) (var x))))))))
   (test-case "valid x86*"
     (check-pred
      x860*?
      '(program
        ()
        ((start .
                (block ()
                       (movq (int 10) (var x))))))))))
