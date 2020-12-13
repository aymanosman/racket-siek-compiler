#lang racket

(provide uncover-live-pass-R1)

(require "live-afters.rkt"
         "raise-mismatch-error.rkt")

(define (uncover-live-pass-R1 p)
  (match p
    [`(program ,info ,code)
     `(program
       ,info
       ,(map
         (match-lambda [(cons label block)
                        (cons label (uncover-live-block block))])
         code))]
    [_ (raise-mismatch-error 'uncover-live-pass-R1 'top p)]))

(define (uncover-live-block b)
  (match b
    [`(block () ,instr* ...)
     `(block
       ((live-afters . ,(live-afters instr*)))
       ,@instr*)]
    [_ (raise-mismatch-error 'uncover-live-pass-R1 'block b)]))
