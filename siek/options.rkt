#lang racket/base

(provide current-system-type
         current-mismatch-handler
         current-x86
         current-type-errors
         compiler-enable-move-biasing?)

;; TODO rename to compiler-system-type
(define current-system-type
  (make-parameter (system-type 'os)))

(define current-mismatch-handler
  (make-parameter
   (lambda (who kind term)
     (raise-arguments-error who
                            "failed to match"
                            "kind"
                            kind
                            "term"
                            term))))

(define current-x86 ;; TODO rename x860
  ;; TODO guard (one of 'x860 'x860*)
  (make-parameter 'x860))

(define compiler-enable-move-biasing?
  (make-parameter #t))

(define current-type-errors
  (make-parameter #f))
