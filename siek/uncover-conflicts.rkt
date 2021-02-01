#lang racket

(provide uncover-conflicts-R1
         uncover-conflicts-R2)

(require "make-conflicts.rkt"
         "raise-mismatch-error.rkt")

(define (uncover-conflicts-R1 p)
  (send (new uncover-conflicts-R1%) conflicts p))

(define uncover-conflicts-R2 uncover-conflicts-R1)

(define uncover-conflicts-R1%
  (class object%
    (super-new)

    (define/public (who)
      'uncover-conflicts-R1)

    (define/public (conflicts p)
      (match p
        [`(program ,info ,blocks)
         `(program
           ,info
           ,(for/list ([b blocks]) (conflicts-block (dict-ref info 'locals) b)))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (conflicts-block locals b)
      (match b
        [(cons label `(block ,info ,instr* ...))
         (define new-info
           (dict-set info 'conflicts (make-conflicts locals
                                                     (dict-ref info 'live-afters)
                                                     instr*)))
         (cons label `(block ,new-info ,@instr*))]
        [_
         (raise-mismatch-error (who) 'block b)]))))
