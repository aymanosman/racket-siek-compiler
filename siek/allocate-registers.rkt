#lang racket

(provide allocate-registers-R1
         allocate-registers-R2)

(require "assign-homes-x86.rkt"
         "raise-mismatch-error.rkt"
         "options.rkt")

(define (allocate-registers-R1 p)
  (send (new allocate-registers-R1%) allocate p))

(define (allocate-registers-R2 p)
  (send (new allocate-registers-R2%) allocate p))

(define allocate-registers-R1%
  (class object%
    (super-new)

    (define/public (who)
      'allocate-registers-R1)

    (define/public (allocate p)
      (match p
        [`(program ,info ,code)
         `(program
           ,info
           ,(map
             (match-lambda
               [(cons label block)
                (cons label (allocate-block block))])
             code))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (allocate-block b)
      (match b
        [`(block ,info ,instr* ...)
         (define colors (dict-ref info 'colors))
         (define homes (colors->homes colors))
         `(block
           ((stack-space . ,(colors->stack-space colors)))
           ,@(assign-homes-x86 homes instr*))]
        [_
         (raise-mismatch-error (who) 'block b)]))))

(define (colors->stack-space colors)
  (define c* (hash-values colors))
  (cond
    [(empty? c*)
     0]
    [else
     (define m (apply max c*))
     (* 8 (max 0 (- m (compiler-stack-location-index))))]))

(define allocate-registers-R2%
  (class allocate-registers-R1%
    (super-new)

    (define/override (who)
      'allocate-registers-R2)))
