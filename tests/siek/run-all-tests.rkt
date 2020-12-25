#lang racket

(require rackunit
         rackunit/text-ui
         "print-x86-pass-R1.rkt"
         "patch-instructions-pass-R1.rkt"
         "allocate-registers-pass-R1.rkt"
         "move-related.rkt"
         "color-graph.rkt"
         "build-interference-pass-R1.rkt"
         "uncover-locals-pass-R1.rkt"
         "assign-homes-pass-R1.rkt"
         "select-instructions-pass-R1.rkt"
         "uncover-live-pass-R1.rkt"
         "explicate-control-pass-R1.rkt"
         "normalize.rkt"
         "uniquify.rkt"
         "interp-x861.rkt"
         "interp-x860.rkt"
         "x860.rkt"
         "C1.rkt"
         "C0.rkt"
         "R2.rkt"
         "R1.rkt"
         "R0.rkt")

(run-tests
 (test-suite "all-tests"
             print-x86-tests
             patch-instructions-tests
             allocate-registers-tests
             move-related-tests
             color-graph-tests
             build-interference-tests
             uncover-live-tests
             assign-homes-tests
             select-instructions-tests
             uncover-locals-tests
             explicate-control-tests
             normalize-R2-tests
             normalize-R1-tests
             uniquify-R2-tests
             uniquify-R1-tests
             interp-x861-tests
             interp-x860-tests
             x860-tests
             interp-C0-tests
             interp-C1-tests
             typecheck-R2-tests
             interp-R2-tests
             interp-R1-tests
             interp-R0-tests))
