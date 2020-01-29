#lang racket

(require rackunit)
(require siek)

;; (return 42)
(check-equal?
 (interp-x860
  '(program ((stack-space . 0))
            ((start . (block ()
                             (movq (int 42) (reg rax))
                             (jmp conclusion))))))
 42)

;; (return (+ 2 1))
(check-equal?
 (interp-x860
  '(program ((stack-space . 0))
            ((start . (block ()
                             (movq (int 2) (reg rax))
                             (addq (int 1) (reg rax))
                             (jmp conclusion))))))
 3)

;; (return (- 10))
(check-equal?
 (interp-x860
  '(program ((stack-space . 0))
            ((start . (block ()
                             (movq (int 10) (reg rax))
                             (negq (reg rax))
                             (jmp conclusion))))))
 -10)


;; (assign x (- 10))
;; (return (+ 52 x))
(check-equal?
 (interp-x860
  '(program ((stack-space . 16))
            ((start . (block ()
                             (movq (int 10) (deref rsp -8))
                             (negq (deref rsp -8))
                             (addq (int 52) (deref rsp -8))
                             (movq (deref rsp -8) (reg rax))
                             (jmp conclusion))))))
 42)


(check-exn #rx"interp-x860: variables are not supported in the current language \\(did you mean to use pseudo-x86\\?\\)\n  current-x86: 'x860\n  variable: 'x"
  (thunk
    (interp-x860
      '(program ((stack-space . 0))
         ((start . (block ()
                          (movq (int 10) (var x))
                          (jmp conclusion))))))))
