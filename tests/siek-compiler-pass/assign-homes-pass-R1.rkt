#lang racket

(require rackunit)

(require siek-compiler-pass/assign-homes-pass-R1)
(require tests/siek-language-tester/check-x860)

(check-x860*=?
 (assign-homes-pass-R1
  '(program ((locals . (tmp.1 tmp.2)))
     ((start . (block ()
                 (movq (int 10) (var tmp.1))
                 (negq (var tmp.1))
                 (movq (var tmp.1) (var tmp.2))
                 (addq (int 52) (var tmp.2))
                 (movq (var tmp.2) (reg rax)))))))

 '(program ((stack-space . 16))
    ((start . (block ()
                (movq (int 10) (deref rbp -8))
                (negq (deref rbp -8))
                (movq (deref rbp -8) (deref rbp -16))
                (addq (int 52) (deref rbp -16))
                (movq (deref rbp -16) (reg rax)))))))
