#lang racket

(provide raise-mismatch-error)

(define (raise-mismatch-error who kind term)
  (raise-arguments-error who
                         "failed to match"
                         "kind"
                         kind
                         "term"
                         term))
