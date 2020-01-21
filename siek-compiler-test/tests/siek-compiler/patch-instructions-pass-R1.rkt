#lang racket

(require rackunit)

(require siek-compiler/patch-instructions-pass-R1)
(require tests/siek-language-tester/check-x860)

(check-x860=?
 (patch-instructions-pass-R1
  '(program ((stack-space . 16))
            ((start . (block ()
                             (movq (int 10) (deref rbp -8))
                             (negq (deref rbp -8))
                             (movq (deref rbp -8) (deref rbp -16))
                             (addq (int 52) (deref rbp -16))
                             (movq (deref rbp -16) (reg rax)))))))

 '(program ((stack-space . 16))
           ((start . (block ()
                            (movq (int 10) (deref rbp -8))
                            (negq (deref rbp -8))
                            (movq (deref rbp -8) (reg rax))
                            (movq (reg rax) (deref rbp -16))
                            (addq (int 52) (deref rbp -16))
                            (movq (deref rbp -16) (reg rax)))))))
