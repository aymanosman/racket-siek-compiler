#lang racket

(provide build-interference-pass-R1)

(require "build-interference.rkt"
         "raise-mismatch-error.rkt")

(define (build-interference-pass-R1 p)
  (match p
    [`(program ,info ,blocks)
     `(program
       ,info
       ,(map build-interference-block blocks))]
    [_
     (raise-mismatch-error 'build-interference-pass-R1 'top p)]))

(define (build-interference-block b)
  (match b
    [(cons label `(block ,info ,instr* ...))
     (cons label
           `(block
             ((conflicts . ,(build-interference (dict-ref info 'live-afters) instr*)))
             ,@instr*))]
    [_
     (raise-mismatch-error 'build-interference-pass-R1 'block b)]))
