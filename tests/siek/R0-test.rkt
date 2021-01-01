#lang racket

(provide interp-R0-tests)

(require siek
         "test-interp.rkt")

(define interp-R0-tests
  (test-interp
   R0
   2
   (- 3)
   (+ (- 3) 2)
   [(read) <= "78"]
   [(- (read)) <= "78"]
   [(+ 1 (+ (read) 100)) <= "8"]
   [(+ 1 (+ (read) 100)) <= "8"]
   [(+ (read) (read)) <= "1 2"]))
