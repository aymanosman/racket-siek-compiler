#lang racket

(provide allocate-registers-pass-R1)

(require graph
         "assign-homes.rkt"
         "color-graph.rkt")

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
     (define conflict (dict-ref info 'conflicts))
     (define colors (color-graph conflict))
     (define homes (colors->homes colors))
     `(block
       ((stack-space . ,(colors->stack-space colors)))
       ,@(assign-homes homes instr*))]))

;; Aux

(define (colors->homes colors)
  (for/hash ([(v c) (in-hash colors)])
    (values v (color->arg c))))

(define k-location (make-parameter 1))

(define (colors->stack-space colors)
  (define c* (hash-values colors))
  (cond
    [(empty? c*)
     0]
    [else
     (define m (apply max c*))
     (* 8 (max 0 (- m (k-location))))]))

(define (color->arg c)
  (cond
    [(and (>= c 0) (< c (k-location)))
     `(reg ,(hash-ref register-table c))]
    [else
     `(deref rbp ,(stack-offset (- c (k-location))))]))

(define (stack-offset n)
  (- (* 8 (add1 n))))

(define register-table
  (hash 0 'rcx))

