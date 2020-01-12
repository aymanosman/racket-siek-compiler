#lang racket

(require rackunit)

(require siek-compiler/remove-complex-opera-pass-R1)

(check-equal?
 (remove-complex-opera*-R1
  '(program ()
            (let ([y (let ([x.1 20])
                       (+ x.1 (let ([x.2 22])
                                x.2)))])
              y)))

 '(program ()
           (let ([y (let ([x.1 20])
                      (let ([x.2 22])
                        (+ x.1 x.2)))])
             y)))

(check-equal?
 (remove-complex-opera*-R1
  '(program ()
            (- (let ([x 10]) x))))
 '(program ()
           (let ([x 10])
             (- x))))

(check-equal?
 (remove-complex-opera*-R1
  '(program () (- (- 10))))
 '(program ()
           (let ([tmp.1 (- 10)])
             (- tmp.1))))

(check-equal?
 (remove-complex-opera*-R1
  '(program ()
            (let ([a 42])
              (let ([b a])
                b))))
 '(program ()
           (let ([a 42])
             (let ([b a])
               b))))
