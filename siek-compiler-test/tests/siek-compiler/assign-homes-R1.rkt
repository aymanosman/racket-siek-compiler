#lang racket

(require rackunit)

(require siek-compiler/uniquify-pass-R1
         siek-compiler/remove-complex-opera-pass-R1
         siek-compiler/explicate-control-pass-R1
         siek-compiler/uncover-locals-pass-R1
         siek-compiler/select-instructions-pass-R1
         siek-compiler/assign-homes-R1)

(define compile (compose1        assign-homes-pass-R1
                          select-instructions-pass-R1
                               uncover-locals-pass-R1
                            explicate-control-pass-R1
                             remove-complex-opera*-R1
                                     uniquify-pass-R1))

(require siek-language/x860
         tests/siek-language-tester/check-x860)

(define-check (check-x860*=? p0 p1)
  (check-x860*? p0)
  (check-x860*? p1)
  (unless (equal? p0 p1)
    (fail-check)))

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