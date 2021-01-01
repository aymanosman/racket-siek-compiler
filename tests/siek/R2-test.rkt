#lang racket

(provide interp-R2-tests
         typecheck-R2-tests)

(require siek
         rackunit
         "test-interp.rkt"
         "test-typecheck.rkt")

(define interp-R2-tests
  (test-interp
   R2
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

(define typecheck-R2-tests
  (test-typecheck-fail
   R2
   (not 1)
   (+ 42 #f)))
