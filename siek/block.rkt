#lang racket

(provide block-info
         block-instr*)

(define (block-info b)
  (match-define `(block ,info ,_ ...) b)
  info)

(define (block-instr* b)
  (match-define `(block ,_ ,instr* ...) b)
  instr*)
