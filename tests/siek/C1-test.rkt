#lang racket

(provide interp-C1-tests)

(require rackunit)

(require siek)

(define-test-suite interp-C1-tests
  (check-equal?
   (interp-C1
    '(program
      ()
      ((start . (return 10)))))
   10)
  (check-equal?
   (interp-C1
    '(program
      ()
      ((start . (return (- 10))))))
   -10)
  (check-equal?
   (interp-C1
    '(program
      ()
      ((start .
              (seq (assign x 10)
                   (return (- x)))))))
   -10)
  (check-equal?
   (interp-C1
    '(program
      ()
      ((start .
              (seq (assign x 10)
                   (seq
                    (assign y 32)
                    (return (+ x y))))))))
   42)
  (check-equal?
   (parameterize ([current-input-port (open-input-string "78")])
     (interp-C1 `(program
                  ()
                  ((start .
                          (seq (assign x (read))
                               (seq
                                (assign y (- -1))
                                (seq
                                 (assign z (+ 1 y))
                                 (return (+ x z))))))))))
   80)
  (check-equal?
   (interp-C1
    '(program
      ()
      ((start .
              (if (< 1 2)
                  (goto then)
                  (goto else)))
       (then . (return 42)))))
   42)
  (check-equal?
   (interp-C1
    '(program
      ()
      ((start .
              (if (> 1 2)
                  (goto then)
                  (goto else)))
       (then . (return 42))
       (else . (return 39)))))
   39))
