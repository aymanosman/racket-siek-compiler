#lang racket

(provide print-x86-tests)

(require siek
         "define-x86-test-suite.rkt")

(define-compiler compile
  (uniquify-R1
   normalize-R1
   explicate-control-R1
   uncover-locals-R1
   select-instructions-R1
   assign-homes-R1
   patch-instructions-R1))

;; (compiler-trace! compile #t)

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


