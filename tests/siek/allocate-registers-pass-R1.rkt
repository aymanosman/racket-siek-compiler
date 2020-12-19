#lang racket

(provide allocate-registers-tests)

(require rackunit
         "check-pass.rkt")

(require siek)

(define compile
  (compose1
   patch-instructions-pass-R1
   allocate-registers-pass-R1
   build-interference-pass-R1
   uncover-live-pass-R1
   select-instructions-pass-R1
   uncover-locals-pass-R1
   explicate-control-pass-R1
   remove-complex-opera*-pass-R1
   uniquify-pass-R1))

(define-test-suite allocate-registers-tests
  ;; TODO rename check-pass* test-compiler
  (check-pass* compile
               (R1 -> x860)
               2
               (- 10)
               (- (+ 10 20))
               (let ([x 32])
                 (+ x 10))
               (let ([x (let ([x 4])
                          (+ x 1))])
                 (+ x 2)))

  (test-case
   "move biasing"
   (check-equal?
    (compile
     '(program ()
               (let ([v 1])
                 (let ([w 42])
                   (let ([x (+ v 7)])
                     (let ([y x])
                       (let ([z (+ x w)])
                         (+ z (- y)))))))))
    '(program ((locals . (v.1 w.2 x.3 y.4 z.5 tmp.6)))
              ((start . (block ((stack-space . 0))
                               (movq (int 1) (reg rcx))
                               (movq (int 42) (reg rbx))
                               ;; (movq (reg rcx) (reg rcx))
                               (addq (int 7) (reg rcx))
                               (movq (reg rcx) (reg rdx))
                               ;; (movq (reg rcx) (reg rcx))
                               (addq (reg rbx) (reg rcx))
                               ;; (movq (reg rdx) (reg rdx))
                               (negq (reg rdx))
                               (movq (reg rcx) (reg rax))
                               (addq (reg rdx) (reg rax))
                               (jmp conclusion))))))))
