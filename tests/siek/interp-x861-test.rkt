#lang racket

(provide interp-x861-tests)

(require rackunit
         siek
         "test-x86.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests interp-x861-tests))

(define-test-suite interp-x861-tests
  (test-x861
   "(return (not #f))"
   1
   (start
    (movq (int 0) (reg rbx))
    (xorq (int 1) (reg rbx))
    (movq (reg rbx) (reg rax))
    (jmp conclusion)))
  (test-x861
   "(return (not #t))"
   0
   (start
    (movq (int 1) (reg rbx))
    (xorq (int 1) (reg rbx))
    (movq (reg rbx) (reg rax))
    (jmp conclusion)))
  (test-x861
   "(return (eq? 3 3))"
   1
   (start
    (movq (int 3) (reg rcx))
    (cmpq (int 3) (reg rcx))
    (sete (bytereg al))
    (movzbq (bytereg al) (reg rax))
    (jmp conclusion)))
  (test-x861
   "(return (< 1 2))"
   1
   (start
    (movq (int 1) (reg rcx))
    (cmpq (int 2) (reg rcx))
    (setl (bytereg al))
    (movzbq (bytereg al) (reg rax))
    (jmp conclusion)))
  (test-x861
   "(return 42)"
   42
   (start
    (movq (int 42) (reg rax))
    (jmp conclusion)))
  (test-x861
   "(return (+ 2 1))"
   3
   (start
    (movq (int 2) (reg rax))
    (addq (int 1) (reg rax))
    (jmp conclusion)))
  (test-x861
   "(return (- 10))"
   -10
   (start
    (movq (int 10) (reg rax))
    (negq (reg rax))
    (jmp conclusion)))
  (test-x861
   "(assign x (- 10))
(return (+ 52 x))"
   42
   (start
    (movq (int 10) (deref rsp -8))
    (negq (deref rsp -8))
    (addq (int 52) (deref rsp -8))
    (movq (deref rsp -8) (reg rax))
    (jmp conclusion)))

  (test-x861
   "(assign x (read)) (return (+ 1 x))"
   #:input
   "11"
   12
   (start
    (callq read_int)
    (addq (int 1) (reg rax))
    (jmp conclusion)))

  (test-x861
   "start:
       if (< 1 2):
         goto then
         goto else
    then:
      return 33"
   33
   (start
    (movq (int 1) (reg rcx))
    (cmpq (int 2) (reg rcx))
    (jl then)
    (jmp else))
   (then
    (movq (int 33) (reg rax))
    (jmp conclusion))
   (else
    (movq (int 98) (reg rax))
    (jmp conclusion)))

  (check-exn
   #rx"interp-x861: failed to match\n  kind: 'l-value\n  term: '\\(var x\\)"
   (thunk
    (interp-x861
     '(program
       ()
       ((start .
               (block ((stack-space . 0))
                      (movq (int 10) (var x))
                      (jmp conclusion)))))))))
