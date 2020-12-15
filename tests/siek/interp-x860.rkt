#lang racket

(provide interp-x860-tests)

(require rackunit)

(require siek)

(define-test-suite interp-x860-tests
  (test-case "(return 42)"
    (check-equal?
     (interp-x860
      '(program
        ()
        ((start .
                (block ((stack-space . 0))
                       (movq (int 42) (reg rax))
                       (jmp conclusion))))))
     42))
  (test-case "(return (+ 2 1))"
    (check-equal?
     (interp-x860
      '(program
        ()
        ((start .
                (block ((stack-space . 0))
                       (movq (int 2) (reg rax))
                       (addq (int 1) (reg rax))
                       (jmp conclusion))))))
     3))
  (test-case "(return (- 10))"
    (check-equal?
     (interp-x860
      '(program
        ()
        ((start .
                (block ((stack-space . 0))
                       (movq (int 10) (reg rax))
                       (negq (reg rax))
                       (jmp conclusion))))))
     -10))
  (test-case "(assign x (- 10))
(return (+ 52 x))"
    (check-equal?
     (interp-x860
      '(program
        ()
        ((start .
                (block ((stack-space . 16))
                       (movq (int 10) (deref rsp -8))
                       (negq (deref rsp -8))
                       (addq (int 52) (deref rsp -8))
                       (movq (deref rsp -8) (reg rax))
                       (jmp conclusion))))))
     42)))

;; TODO why is this broken?
#;
(check-exn #rx"interp-x860: failed to match\n  kind: 'l-value\n  term: '(var x)"
           (thunk
            (interp-x860
             '(program
               ((stack-space . 0))
               ((start .
                       (block ()
                              (movq (int 10) (var x))
                              (jmp conclusion))))))))
