#lang racket/base

(provide current-system-type
         compiler-enable-move-biasing?)

;; TODO rename to compiler-system-type
(define current-system-type
  (make-parameter (system-type 'os)))

(define compiler-enable-move-biasing?
  (make-parameter #t))
