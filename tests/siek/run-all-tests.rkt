#lang racket

(require rackunit
         rackunit/text-ui
         "x860.rkt"
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
         "remove-complex-opera-pass-R1.rkt"
         "uniquify-pass-R1.rkt"
         "interp-x860.rkt")

(run-tests
 (test-suite "all-tests"
             x860-tests
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
             remove-complex-opera*-tests
             uniquify-tests
             interp-x860-tests))
