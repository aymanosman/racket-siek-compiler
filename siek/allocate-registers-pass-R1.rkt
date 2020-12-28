#lang racket

(provide allocate-registers-pass-R1)

(require "assign-homes.rkt"
         "color-homes.rkt"
         "options.rkt")

(define (allocate-registers-pass-R1 p)
  (match p
    [`(program ,info ,code)
     `(program
       ,info
       ,(map
         (match-lambda
           [(cons label block)
            (cons label (allocate-block block))])
         code))]))

(define (allocate-block b)
  (match b
    [`(block ,info ,instr* ...)
     (define colors (dict-ref info 'colors))
     (define homes (colors->homes colors))
     `(block
       ((stack-space . ,(colors->stack-space colors)))
       ,@(assign-homes homes instr*))]))

(define (colors->stack-space colors)
  (define c* (hash-values colors))
  (cond
    [(empty? c*)
     0]
    [else
     (define m (apply max c*))
     (* 8 (max 0 (- m (compiler-stack-location-index))))]))
