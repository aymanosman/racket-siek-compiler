#lang racket

(provide interp-R1-tests)

(require siek
         "test-interpreter.rkt")

(define interp-R1-tests
  (test-interpreter
   interp-R1
   [(let ([x (read)])
      (+ x 2))
    <=
    "3"]
   (let ([x (let ([y 10])
              y)])
     (+ x 2))
   2
   (- 3)
   (+ (- 3) 2)
   [(read) <= "78"]
   [(- (read)) <= "78"]
   [(+ 1 (+ (read) 100)) <= "8"]
   [(+ 1 (+ (read) 100)) <= "8"]
   [(+ (read) (read)) <= "1 2"]))
