#lang racket/base

(provide current-system-type)

(define current-system-type
  (make-parameter (system-type 'os)))
