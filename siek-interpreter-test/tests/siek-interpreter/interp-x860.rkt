#lang racket

(require rackunit)
(require siek-interpreter/interp-x860)

;; (return 42)
(check-equal?
 (interp-x860
  '(program ()
            ((start . (block ()
                             (movq (int 42) (reg rax)))))))
 42)

;; (return (+ 2 1))
(check-equal?
 (interp-x860
  '(program ()
            ((start . (block ()
                             (movq (int 2) (reg rax))
                             (addq (int 1) (reg rax)))))))
 3)

;; (return (- 10))
(check-equal?
 (interp-x860
  '(program ()
            ((start . (block ()
                             (movq (int 10) (reg rax))
                             (negq (reg rax)))))))
 -10)


;; (assign x (- 10))
;; (return (+ 52 x))
(check-equal?
 (interp-x860
  '(program ()
            ((start . (block ()
                             (movq (int 10) (deref rsp -8))
                             (negq (deref rsp -8))
                             (addq (int 52) (deref rsp -8))
                             (movq (deref rsp -8) (reg rax)))))))
 42)
