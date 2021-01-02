#lang racket

(provide explicate-control-tests
         explicate-control-R2-tests)

(require siek
         "define-compiler-test-suite.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests explicate-control-tests)
  (run-tests explicate-control-R2-tests))

(define-compiler compile-R1
  (uniquify-R1
   normalize-R1
   explicate-control-R1))

(define-compiler-test-suite explicate-control-tests
  #:compiler compile-R1
  #:signature (R1 -> C0)
  2
  [(read) <= "1"]
  (let ([x 32])
    (+ x 10))
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2)))

(define-compiler compile-R2
  (typecheck-R2
   shrink-R2
   uniquify-R2
   normalize-R2
   explicate-control-R2))

(compiler-trace! compile-R2 #t)

(define-extended-compiler-test-suite explicate-control-R2-tests explicate-control-tests
  #:compiler compile-R2
  #:signature (R2 -> C1)
  [(- (read) (read)) <= "13 3"]
  (if #t
      1
      2)
  (if #f
      1
      2)
  (- 10 20)
  (if (< 1 2)
      1
      2)
  (if (not (< 1 2))
      1
      2)
  (if (if (< 1 2) #t #f)
      12
      13)
  (let ([x (if (< 1 2)
               10
               20)])
    (+ x 100))
  [(let ([x (if (< (read) (read))
                10
                20)])
     (+ x 100))
   <= "2 3"])
