#lang racket

(provide raise-mismatch-error)

(require "options.rkt")

(define (raise-mismatch-error who kind term)
  ((current-mismatch-handler) who kind term))
