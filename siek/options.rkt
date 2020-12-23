#lang racket/base

(provide current-system-type
         current-mismatch-handler
         current-type-errors
         compiler-psuedo-x86?
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

(define compiler-psuedo-x86?
  (make-parameter #f))

(define compiler-enable-move-biasing?
  (make-parameter #t))

(define current-type-errors
  (make-parameter #f))
