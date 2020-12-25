#lang racket

(provide fresh
         current-gensym
         make-gensym)

(define (fresh [x 'tmp])
  ((current-gensym) x))

(define (make-gensym [n 1])
  (lambda ([x 'tmp])
    (begin0
        (string->symbol (format "~a.~a" x n))
      (set! n (add1 n)))))

(define current-gensym (make-parameter (make-gensym)))
