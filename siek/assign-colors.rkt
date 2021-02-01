#lang racket

(provide assign-colors-R1
         assign-colors-R2)

(require "color-graph.rkt"
         "move-related.rkt"
         "raise-mismatch-error.rkt"
         "options.rkt")

(define (assign-colors-R1 p)
  (send (new assign-colors-R1%) assign p))

(define assign-colors-R2 assign-colors-R1)

(define assign-colors-R1%
  (class object%
    (super-new)

    (define/public (who)
      'assign-colors-R1)

    (define/public (assign p)
      (match p
        [`(program ,info ,code)
         `(program
           ,info
           ,(map
             (match-lambda
               [(cons label block)
                (cons label (assign-block (dict-ref info 'locals) block))])
             code))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (assign-block locals b)
      (match b
        [`(block ,info ,instr* ...)
         `(block
           ,(dict-set info 'colors (color-graph (dict-ref info 'conflicts)
                                                locals
                                                (and (compiler-enable-move-biasing?)
                                                     (move-related locals instr*))))
           ,@instr*)]
        [_
         (raise-mismatch-error (who) 'block b)]))))
