#lang racket

(require rackunit)
(require siek-interpreter/interp-R1)

(check-equal? (interp-R1 `(program () 42))
              42)

(check-equal? (interp-R1 `(program () (- 10)))
              -10)

(check-equal? (interp-R1 `(program () (+ 10 (+ 2 30))))
              42)

(check-equal? (parameterize ([current-input-port (open-input-string "78")])
                (interp-R1 `(program () (+ (read) (+ 1 (- -1))))))
              80)

(check-equal? (parameterize ([current-input-port (open-input-string "52 10")])
                (interp-R1 `(program ()
                              (let ([x (read)])
                                (let ([y (read)])
                                  (+ x (- y)))))))
              42)


(require "private/test-interp.rkt")

(test-interp* interp-R1
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
(test-interp* interp-R1
  2
  (- 3)
  (+ (- 3) 2)
  [(read) <= "78"]
  [(- (read)) <= "78"]
  [(+ 1 (+ (read) 100)) <= "8"]
  [(+ 1 (+ (read) 100)) <= "8"]
  [(+ (read) (read)) <= "1 2"])
