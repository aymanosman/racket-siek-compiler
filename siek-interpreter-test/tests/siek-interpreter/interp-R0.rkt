#lang racket

(require rackunit)

(require siek-interpreter/interp-R0)

(check-equal? (interp-R0 `(program () 42))
              42)

(check-equal? (interp-R0 `(program () (- 10)))
              -10)

(check-equal? (interp-R0 `(program () (+ 10 (+ 2 30))))
              42)

(check-equal? (parameterize ([current-input-port (open-input-string "78")])
                (interp-R0 `(program () (+ (read) (+ 1 (- -1))))))
              80)

(check-exn #rx"interp-R0: contract violation\n  expected: integer\\?\n  given: 'foo"
  (thunk
    (parameterize ([current-input-port (open-input-string "foo")])
      (interp-R0 `(program () (+ (read) (+ 1 (- -1))))))))

(require "private/test-interp.rkt")

(test-interp* interp-R0
  2
  (- 3)
  (+ (- 3) 2)
  [(read) <= "78"]
  [(- (read)) <= "78"]
  [(+ 1 (+ (read) 100)) <= "8"]
  [(+ 1 (+ (read) 100)) <= "8"]
  [(+ (read) (read)) <= "1 2"])