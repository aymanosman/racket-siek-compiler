#lang racket

(provide interp-x860-tests)

(require rackunit
         siek
         "test-x86.rkt")

(define-test-suite interp-x860-tests
  (parameterize ([compiler-psuedo-x86? #t])
    (test-x860
     "(return 42)"
     42
     (start
      (movq (int 42) (reg rax))
      (jmp conclusion))))
  (test-x860
   "(return 42)"
   42
   (start
    (movq (int 42) (reg rax))
    (jmp conclusion)))
  (test-x860
   "(return (+ 2 1))"
   3
   (start
    (movq (int 2) (reg rax))
    (addq (int 1) (reg rax))
    (jmp conclusion)))
  (test-x860
   "(return (- 10))"
   -10
   (start
    (movq (int 10) (reg rax))
    (negq (reg rax))
    (jmp conclusion)))
  (test-x860
   "(assign x (- 10))
(return (+ 52 x))"
   42
   (start
    (movq (int 10) (deref rsp -8))
    (negq (deref rsp -8))
    (addq (int 52) (deref rsp -8))
    (movq (deref rsp -8) (reg rax))
    (jmp conclusion)))
  (test-x860
   "(assign x (read)) (return (+ 1 x))"
   #:input
   "11"
   12
   (start
    (callq read_int)
    (addq (int 1) (reg rax))
    (jmp conclusion)))
  (check-exn
   #rx"interp-x860: failed to match\n  kind: 'l-value\n  term: '\\(var x\\)"
   (thunk
    (interp-x860
     '(program
       ()
       ((start .
               (block ((stack-space . 0))
                      (movq (int 10) (var x))
                      (jmp conclusion)))))))))
