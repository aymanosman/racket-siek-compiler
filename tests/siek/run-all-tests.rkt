#lang racket

(require rackunit
         rackunit/text-ui
         "print-x86-test.rkt"
         "patch-instructions-test.rkt"
         "move-bias-test.rkt"
         "allocate-registers-test.rkt"
         "move-related-test.rkt"
         "color-graph-test.rkt"
         "build-interference-test.rkt"
         "uncover-locals-test.rkt"
         "assign-homes-test.rkt"
         "select-instructions-test.rkt"
         "explicate-control-test.rkt"
         "normalize-test.rkt"
         "uniquify-test.rkt"
         "interp-x861-test.rkt"
         "interp-x860-test.rkt"
         "x860-test.rkt"
         "C-test.rkt"
         "R2-test.rkt"
         "R1-test.rkt"
         "R0-test.rkt")

(run-tests
 (test-suite "all-tests"
             print-x86-tests
             patch-instructions-tests
             move-bias-tests
             allocate-registers-tests
             move-related-tests
             color-graph-tests
             build-interference-tests
             assign-homes-tests
             select-instructions-tests
             uncover-locals-tests
             explicate-control-tests
             explicate-control-R2-tests
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
