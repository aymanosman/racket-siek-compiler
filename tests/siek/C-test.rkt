#lang racket

(provide interp-C0-tests
         interp-C1-tests)

(require siek
         "test-C.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests interp-C0-tests)
  (run-tests interp-C1-tests))

(define-interp-test-suite interp-C0-tests
  #:interpreter interp-C0
  
  (test
   #:expect 10
   (start
    (return 10)))

  (test
   #:expect -10
   (start
    (return (- 10))))

  (test
   #:expect -10
   (start
    (assign x 10)
    (return (- x))))

  (test
   #:expect 42
   (start
    (assign x 10)
    (assign y 32)
    (return (+ x y))))

  (test
   #:input "78"
   #:expect 80
   (start
    (assign x (read))
    (assign y (- -1))
    (assign z (+ 1 y))
    (return (+ x z)))))

(define-extended-interp-test-suite interp-C1-tests interp-C0-tests
  #:interpreter interp-C1
  
  (test
   #:expect 42
   (start
    (if (< 1 2)
        (goto then)
        (goto else)))
   (then
    (return 42)))

  (test
   #:expect 39
   (start
    (if (> 1 2)
        (goto then)
        (goto else)))
   (else
    (return 39))))
