#lang racket

(require rackunit
         "test-interp.rkt")

(require siek)

(test-interp* interp-R2
  ;; 1)
  [(let ([x (read)])
     (+ x 2))
    <=
    "3"]
  ;; 2)
  (let ([x (let ([y 10])
             y)])
    (+ x 2)))

;; include R0 tests
(test-interp* interp-R2
  2
  (- 3)
  (+ (- 3) 2)
  [(read) <= "78"]
  [(- (read)) <= "78"]
  [(+ 1 (+ (read) 100)) <= "8"]
  [(+ 1 (+ (read) 100)) <= "8"]
  [(+ (read) (read)) <= "1 2"])
