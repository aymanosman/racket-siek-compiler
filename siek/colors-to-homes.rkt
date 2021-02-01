#lang racket

(provide colors->homes
         color=>register
         register=>color)

(require "options.rkt")

(define (colors->homes colors)
  (define (color->arg c)
    (cond
      [(and (>= c 0) (< c (compiler-stack-location-index)))
       `(reg ,(hash-ref color=>register c))]
      [else
       `(deref rbp ,(stack-offset (- c (compiler-stack-location-index))))]))

  (define (stack-offset n)
    (- (* 8 (add1 n))))

  (for/hash ([(v c) (in-hash colors)])
    (values v (color->arg c))))

(define color=>register
  (hash 0 'rbx
        1 'rcx ;; caller-saved
        2 'rdx ;; caller-saved

        3 'r12
        4 'r13
        5 'r14
        6 'r15

        7 'rsi
        8 'rdi
        9 'r8
        10 'r9
        11 'r10
        12 'r11))

(define register=>color
  (for/hash ([(k v) (in-hash color=>register)])
    (values v k)))
