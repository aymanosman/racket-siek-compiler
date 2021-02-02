#lang racket

(provide select-instructions-pass-tests
         select-instructions-tests
         select-instructions-R2-tests)

(require siek
         rackunit
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests select-instructions-pass-tests)
  (run-tests select-instructions-tests)
  (run-tests select-instructions-R2-tests))

(define-test-suite select-instructions-pass-tests
  (test-case "(return (eq? 1 2))"
    (check-equal? (select-instructions-R2 '(program () ((start . (return (eq? 1 2))))))
                  '(program
                    ()
                    ((start . (block ()
                                     (movq (int 1) (reg rax))
                                     (cmpq (int 2) (reg rax))
                                     (sete (bytereg al))
                                     (movzbq (bytereg al) (reg rax))
                                     (jmp conclusion))))))))

(define-compiler compiler-R1
  (uniquify-R1
   normalize-R1
   explicate-control-R1
   remove-jumps-R1
   uncover-locals-R1
   select-instructions-R1))

(define-compiler-test-suite select-instructions-tests
  #:compiler compiler-R1
  #:signature (R1 -> x860*)
  2
  (- 10)
  (- (+ 10 20))
  [(read) <= "7"]
  [(+ (read) (read)) <= "32 20"]
  (let ([x 32])
    (+ x 10))
  (let ([x (+ 20 12)])
    (+ x 10))
  (let ([x 32])
    (let ([y (+ 10 x)])
      x))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))

(define-compiler compiler-R2
  (typecheck-R2
   shrink-R2
   uniquify-R2
   normalize-R2
   explicate-control-R2
   remove-jumps-R2
   uncover-locals-R2
   select-instructions-R2))

;; (compiler-trace! compiler-R2 #t)

(define-extended-compiler-test-suite select-instructions-R2-tests select-instructions-tests
  #:compiler compiler-R2
  #:signature (R2 -> x861*)
  (- 10 29)
  (if (eq? 1 2)
      10
      20)
  (if (not (eq? 1 2))
      10
      20)
  (if (not (not #t))
      10
      20)
  (if (not (not #f))
      10
      20)
  (if (< 1 2)
      10
      20)
  (if (or #f #t)
      10
      20)
  (if (<= 1 2)
      10
      20)
  (if (>= 1 2)
      10
      20)
  (let ([x (eq? 1 2)])
    (if x
        10
        20))
  (let ([x (not (eq? 1 2))])
    (if x
        10
        20)))
