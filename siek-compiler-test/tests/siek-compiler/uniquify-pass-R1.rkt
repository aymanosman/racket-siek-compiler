#lang racket

(require rackunit)

(require siek-compiler/uniquify-pass-R1
         siek-compiler/gensym)

(check-equal? (uniquify-pass-R1 `(program () 42))
              '(program () 42))

(check-equal? (uniquify-pass-R1 `(program () (read)))
              '(program () (read)))

(check-equal? (uniquify-pass-R1 `(program () (- (read))))
              '(program () (- (read))))

(check-equal? (uniquify-pass-R1 `(program () (+ (- 10) (read))))
              '(program () (+ (- 10) (read))))

(check-equal? (uniquify-pass-R1 `(program () (+ x (read))))
              '(program () (+ x (read))))

(check-equal? (parameterize ([current-gensym (make-gensym)])
                (uniquify-pass-R1
                 `(program ()
                    (let ([x 32])
                      (+ x (read))))))
              '(program ()
                 (let ([x.1 32])
                   (+ x.1 (read)))))

(check-equal? (parameterize ([current-gensym (make-gensym)])
                (uniquify-pass-R1
                 `(program ()
                    (let ([x 32])
                      (+ (let ([x 10]) x) x)))))
              `(program ()
                 (let ([x.1 32])
                   (+ (let ([x.2 10]) x.2) x.1))))

(check-equal? (parameterize ([current-gensym (make-gensym)])
                (uniquify-pass-R1
                 `(program ()
                    (let ([x (let ([x 4])
                               (+ x 1))])
                      (+ x 2)))))
              `(program ()
                 (let ([x.2 (let ([x.1 4])
                              (+ x.1 1))])
                   (+ x.2 2))))
