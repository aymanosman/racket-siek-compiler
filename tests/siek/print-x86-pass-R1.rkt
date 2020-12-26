#lang racket

(provide print-x86-tests)

(require siek
         "define-x86-test-suite.rkt")

(define-compiler compile
  (uniquify-R1
   normalize-R1
   explicate-control-pass-R1
   uncover-locals-pass-R1
   select-instructions-pass-R1
   assign-homes-pass-R1
   patch-instructions-pass-R1))

(compiler-trace! compile #t)

(define-x86-test-suite print-x86-tests compile
                       2
                       (+ 10 20)
                       (+ 52 (- 10))
                       [(read) <= "78"]
                       [(+ (read) (read)) <= "1 2"]
                       (let ([x 32])
                         (+ x 10))
                       (let ([x (let ([x 4])
                                  (+ x 1))])
                         (+ x 2)))


