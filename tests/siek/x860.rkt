#lang racket

(provide x860-tests)

(require rackunit)

(require "check-x860.rkt")

(define normal-x86-tests
  (test-suite
   "normal x86"
   (test-case "valid x86"
     (check-x860?
      '(program
        ()
        ((start .
                (block ()
                       (movq (int 10) (reg rax))))))))
   (test-case "valid x86"
     (check-not-x860?
      '(program
        ()
        ((start .
                (block ()
                       (movq (int 10) (var x))))))))))

(define psuedo-x86-tests
  (test-suite
   "pseudo x86"
   (test-case "valid x86*"
     (check-x860*?
      '(program
        ()
        ((start .
                (block ()
                       (movq (int 10) (var x))))))))))

(define-test-suite x860-tests
  (test-suite "x86"
              normal-x86-tests
              psuedo-x86-tests))
