#lang racket

(provide build-interference-pass-R1)

(require "build-interference.rkt")

(define (build-interference-pass-R1 p)
  (match p
    [`(program ,info ,blocks)
     `(program
       ,info
       ,(map build-interference-block blocks))]
    [_
     (report-mismatch-error 'top p)]))

(define (build-interference-block b)
  (match b
    [(cons label `(block ,info ,instr* ...))
     (cons label
           `(block
             ((conflicts . ,(build-interference (dict-ref info 'live-afters) instr*)))
             ,@instr*))]
    [_
     (report-mismatch-error 'block b)]))

;; Aux

(define (report-mismatch-error kind term)
  (raise-arguments-error 'build-interference-pass-R1
                         "failed match"
                         "kind"
                         kind
                         "term"
                         term))
