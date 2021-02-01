#lang racket

(provide print-x86-tests
         print-x86-R1-registers-tests
         print-x86-R2-tests)

(require siek
         "define-x86-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests print-x86-tests)
  (run-tests print-x86-R1-registers-tests)
  (run-tests print-x86-R2-tests))

(define-compiler compiler-R1
  (uniquify-R1
   normalize-R1
   explicate-control-R1
   remove-jumps-R1
   uncover-locals-R1
   select-instructions-R1
   assign-homes-R1
   patch-instructions-R1))

;; (compiler-trace! compiler-R1 #t)

(define-x86-test-suite print-x86-tests compiler-R1
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

(define-compiler compiler-R1-registers
  (uniquify-R1
   normalize-R1
   explicate-control-R1
   remove-jumps-R1
   uncover-locals-R1
   select-instructions-R1
   uncover-live-R1
   uncover-conflicts-R1
   assign-colors-R1
   allocate-registers-R1
   patch-instructions-R1))

;; (compiler-trace! compiler-R1-registers #t)

(define-x86-test-suite print-x86-R1-registers-tests compiler-R1-registers
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

(define-compiler compiler-R2
  (typecheck-R2
   shrink-R2
   uniquify-R2
   normalize-R2
   explicate-control-R2
   remove-jumps-R2
   uncover-locals-R2
   select-instructions-R2
   uncover-live-R2
   uncover-conflicts-R2
   assign-colors-R2
   allocate-registers-R2
   patch-instructions-R2))

;; (compiler-trace! compiler-R2 #t)

(define-x86-test-suite print-x86-R2-tests compiler-R2
  2
  (+ 10 20)
  (+ 52 (- 10))
  [(read) <= "78"]
  [(+ (read) (read)) <= "1 2"]
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2))
  (let ([v 10])
    (let ([w 20])
      (if (if (let ([x 1]) (< x 2)) (eq? 1 2) #f)
          (+ v 1)
          (+ w 1)))))
