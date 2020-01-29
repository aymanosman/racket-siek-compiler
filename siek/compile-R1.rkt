#lang racket

(provide compile-R1)

(require "uniquify-pass-R1.rkt"
         "remove-complex-opera-pass-R1.rkt"
         "explicate-control-pass-R1.rkt"
         "uncover-locals-pass-R1.rkt"
         "select-instructions-pass-R1.rkt"
         "assign-homes-pass-R1.rkt"
         "patch-instructions-pass-R1.rkt"
         "print-x86-pass-R1.rkt")

(define passes/chapter2
  (list patch-instructions-pass-R1
        assign-homes-pass-R1
        select-instructions-pass-R1
        uncover-locals-pass-R1
        explicate-control-pass-R1
        remove-complex-opera*-pass-R1
        uniquify-pass-R1))

(define passes/chapter3
  (list patch-instructions-pass-R1
        ;; allocate-registers-pass-R1
        ;; build-interference-graph-pass-R1
        ;; uncover-live-pass-R1
        select-instructions-pass-R1
        uncover-locals-pass-R1
        explicate-control-pass-R1
        remove-complex-opera*-pass-R1
        uniquify-pass-R1))

(define R1->x860/chapter2
  (apply compose1 passes/chapter2))

(define compile-R1
  (compose1 print-x86-pass-R1
            R1->x860/chapter2))

(module+ main
  (require "options.rkt")

  (define expr (read))

  (parameterize ([current-system-type (system-type 'os)])
    (compile-R1 `(program () ,expr))))
