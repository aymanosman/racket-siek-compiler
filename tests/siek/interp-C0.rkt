#lang racket

(require rackunit)

(require siek)

(check-equal?
  (interp-C0
    '(program ()
       ((start . (return 10)))))
  10)

(check-equal?
  (interp-C0
    '(program ()
       ((start . (return (- 10))))))
  -10)

(check-equal?
    (interp-C0
    '(program ()
        ((start . (seq (assign x 10) 
                       (return (- x)))))))
    -10)


(check-equal?
    (interp-C0
    '(program ()
        ((start . (seq (assign x 10)
                       (seq
                         (assign y 32)
                         (return (+ x y))))))))
    42)

(check-equal? 
  (parameterize ([current-input-port (open-input-string "78")])
    (interp-C0 `(program () 
                  ((start . (seq (assign x (read))
                                 (seq 
                                   (assign y (- -1))
                                    (seq 
                                      (assign z (+ 1 y))
                                      (return (+ x z))))))))))
    80)