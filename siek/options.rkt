#lang racket/base

(provide current-system-type
         current-mismatch-handler
         current-type-errors
         (rename-out [current-gensym current-compiler-gensym]
                     [make-gensym make-compiler-gensym])
         fresh
         compiler-psuedo-x86?
         compiler-stack-location-index
         compiler-enable-move-biasing?
         compiler-raise-exception-on-type-error)

(require "gensym.rkt")

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

(define current-type-errors (make-parameter #f))

(define compiler-psuedo-x86? (make-parameter #f))

(define compiler-enable-move-biasing? (make-parameter #t))

(define compiler-stack-location-index (make-parameter 3))

(define compiler-raise-exception-on-type-error (make-parameter #t))
