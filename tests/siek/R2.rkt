#lang racket

(provide interp-R2-tests)

(require siek
         "test-interpreter.rkt")

(define interp-R2-tests
  (test-interpreter
   interp-R2
   (- 5 3)
   #t
   #f
   (< 1 2)
   (>= (+ 1 10) (- 8))
   (and (and #t #t) #t)
   (or #f #t)
   (if (< 0 1)
       39
       10)
   (if (if (< 0 1) #f #f)
       39
       10)
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
