#lang racket

(require rackunit)

(require siek-compiler-pass/uncover-locals-pass-R1)

(check-equal?
  (uncover-locals-pass-R1
   '(program ()
      ((start . (seq (assign x.1 20)
                     (seq (assign x.2 22)
                          (seq (assign y (+ x.1 x.2))
                               (return y))))))))

  '(program ((locals . (x.1 x.2 y)))
     ((start . (seq (assign x.1 20)
                    (seq (assign x.2 22)
                         (seq (assign y (+ x.1 x.2))
                              (return y))))))))
