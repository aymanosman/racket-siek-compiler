#lang racket

(provide uncover-live-pass-R1)

(require "live-afters.rkt")

(define (uncover-live-pass-R1 p)
  (match p
    [`(program ,info ,code)
     `(program
       ,info
       ,(map
         (match-lambda [(cons label block)
                        (cons label (uncover-live-block block))])
         code))]
    [_ (report-mismatch-error 'top p)]))

(define (uncover-live-block b)
  (match b
    [`(block () ,instr* ...)
     `(block
       ((live-afters . ,(live-afters instr*)))
       ,@instr*)]
    [_ (report-mismatch-error 'block b)]))

;; Aux

(define (report-mismatch-error kind term)
  (raise-arguments-error 'uncover-live-pass-R1
                         "failed match"
                         "kind"
                         kind
                         "term"
                         term))
