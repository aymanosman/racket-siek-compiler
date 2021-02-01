#lang racket

(provide uncover-live-R1
         uncover-live-R2)

(require "live-afters.rkt"
         "raise-mismatch-error.rkt")

(define (uncover-live-R1 p)
  (send (new uncover-live-R1%) live p))

(define (uncover-live-R2 p)
  (send (new uncover-live-R2%) live p))

(define uncover-live-R1%
  (class object%
    (super-new)

    (define/public (who)
      'uncover-live-R1)

    (define/public (live p)
      (match p
        [`(program ,info ,code)
         (define live-env (live-afters code))
         (define new-code
           (map (match-lambda
                  [(cons label `(block ,info ,instr* ...))
                   (define new-info (dict-set info 'live-afters (dict-ref live-env label)))
                   (cons label `(block ,new-info ,@instr*))])
                code))
         `(program ,info ,new-code)]
        [_ (raise-mismatch-error (who) 'top p)]))))

(define uncover-live-R2%
  (class uncover-live-R1%
    (super-new)

    (define/override (who)
      'uncover-live-R2)))
