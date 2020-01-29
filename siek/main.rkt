#lang racket

(define-syntax-rule (provide/require module-path)
  (begin
    (provide (all-from-out module-path))
    (require module-path)))

(provide/require "R0.rkt")
(provide/require "R1.rkt")
(provide/require "x860.rkt")

(provide/require "interp-R0.rkt")
(provide/require "interp-R1.rkt")
(provide/require "interp-C0.rkt")
(provide/require "interp-x860.rkt")

(provide/require "uniquify-pass-R1.rkt")
(provide/require "remove-complex-opera-pass-R1.rkt")
(provide/require "explicate-control-pass-R1.rkt")
(provide/require "uncover-locals-pass-R1.rkt")
(provide/require "select-instructions-pass-R1.rkt")
(provide/require "assign-homes-pass-R1.rkt")
(provide/require "patch-instructions-pass-R1.rkt")
(provide/require "print-x86-pass-R1.rkt")
(provide/require "uncover-live-pass-R1.rkt")
