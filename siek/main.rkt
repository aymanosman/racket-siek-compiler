#lang racket

(define-syntax-rule (provide/require module-path)
  (begin
    (provide (all-from-out module-path))
    (require module-path)))

(provide/require "options.rkt")

(provide/require "R0.rkt")
(provide/require "R1.rkt")
(provide/require "R2.rkt")
(provide/require "C0.rkt")
(provide/require "C1.rkt")
(provide/require "x860.rkt")
(provide/require "x861.rkt")

(provide/require "define-compiler.rkt")

(provide/require "typecheck-R0.rkt")
(provide/require "typecheck-R1.rkt")
(provide/require "typecheck-R2.rkt")
(provide/require "shrink-R2.rkt")
(provide/require "uniquify-R1.rkt")
(provide/require "uniquify-R2.rkt")
(provide/require "normalize-R1.rkt")
(provide/require "normalize-R2.rkt")
(provide/require "explicate-control-pass-R1.rkt")
(provide/require "explicate-control-R2.rkt")
(provide/require "uncover-locals-pass-R1.rkt")
(provide/require "select-instructions-pass-R1.rkt")
(provide/require "assign-homes-pass-R1.rkt")
(provide/require "patch-instructions-pass-R1.rkt")
(provide/require "print-x86-pass-R1.rkt")
(provide/require "uncover-live-pass-R1.rkt")
(provide/require "build-interference-pass-R1.rkt")
(provide/require "assign-colors-R1.rkt")
(provide/require "allocate-registers-pass-R1.rkt")

(require racket/runtime-path)

(define-runtime-path runtime.c "runtime.c")

(provide runtime.c)
