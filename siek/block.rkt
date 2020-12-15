#lang racket

(provide block-info)

(define (block-info b)
  (match-define `(block ,info ,instr* ...) b)
  info)
