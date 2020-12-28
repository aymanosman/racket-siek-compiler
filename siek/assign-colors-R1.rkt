#lang racket

(provide assign-colors-R1)

(require "color-graph.rkt"
         "move-related.rkt"
         "options.rkt")

(define (assign-colors-R1 p)
  (match p
    [`(program ,info ,code)
     `(program
       ,info
       ,(map
         (match-lambda
           [(cons label block)
            (cons label (assign-colors-block block))])
         code))]))

(define (assign-colors-block b)
  (match b
    [`(block ,info ,instr* ...)
     `(block
       ,(dict-set info 'colors (color-graph (dict-ref info 'conflicts)
                                            (and (compiler-enable-move-biasing?) (move-related instr*))))
       ,@instr*)]))
