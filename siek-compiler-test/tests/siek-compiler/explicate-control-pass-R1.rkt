#lang racket

(require rackunit)

(require siek-compiler/explicate-control-pass-R1)

(check-equal?
 (explicate-control-pass-R1
  `(program () 10))
 '(program ()
           ((start . (return 10)))))

(check-equal?
 (explicate-control-pass-R1
  `(program () x))
 '(program ()
           ((start . (return x)))))

(check-equal?
 (explicate-control-pass-R1
  `(program ()
            (let ([y (- 10)])
              y)))
 '(program ()
           ((start .
                   (seq (assign y (- 10))
                        (return y))))))

(check-equal?
 (explicate-control-pass-R1
  `(program ()
            (let ([y (+ 10 32)])
              y)))
 '(program ()
           ((start .
                   (seq (assign y (+ 10 32))
                        (return y))))))

(check-equal?
 (explicate-control-pass-R1
  `(program ()
            (let ([y (let ([x.1 20])
                       (let ([x.2 22])
                         (+ x.1 x.2)))])
              y)))
 '(program ()
           ((start .
                   (seq (assign x.1 20)
                        (seq (assign x.2 22)
                             (seq (assign y (+ x.1 x.2))
                                  (return y))))))))
