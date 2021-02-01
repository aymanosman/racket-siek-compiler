#lang racket

(define-syntax-rule (provide/require module-path)
  (begin
    (provide (all-from-out module-path))
    (require module-path)))

(provide/require "options.rkt")

(provide/require "R.rkt")
(provide/require "C.rkt")
(provide/require "x86.rkt")

(provide/require "define-compiler.rkt")

(provide/require "typecheck-R.rkt")
(provide/require "shrink.rkt")
(provide/require "uniquify.rkt")
(provide/require "normalize.rkt")
(provide/require "explicate-control.rkt")
(provide/require "remove-jumps.rkt")
(provide/require "uncover-locals.rkt")
(provide/require "select-instructions.rkt")
(provide/require "assign-homes.rkt")
(provide/require "patch-instructions.rkt")
(provide/require "print-x86.rkt")
(provide/require "uncover-live.rkt")
(provide/require "uncover-conflicts.rkt")
(provide/require "assign-colors.rkt")
(provide/require "allocate-registers.rkt")

(require racket/runtime-path)

(define-runtime-path runtime.c "runtime.c")

(provide runtime.c)
