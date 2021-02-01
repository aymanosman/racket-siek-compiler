#lang racket

(require rackunit
         rackunit/text-ui
         "inspect-test.rkt"
         "print-x86-test.rkt"
         "patch-instructions-test.rkt"
         "move-bias-test.rkt"
         "allocate-registers-test.rkt"
         "move-related-test.rkt"
         "color-graph-test.rkt"
         "live-afters-test.rkt"
         "make-conflicts-test.rkt"
         "uncover-locals-test.rkt"
         "select-instructions-test.rkt"
         "explicate-control-test.rkt"
         "normalize-test.rkt"
         "uniquify-test.rkt"
         "interp-x861-test.rkt"
         "interp-x860-test.rkt"
         "C-test.rkt"
         "R-test.rkt")

(run-tests
 (test-suite "all-tests"
             inspect-tests
             print-x86-tests
             print-x86-R1-registers-tests
             print-x86-R2-tests
             patch-instructions-tests
             patch-instructions-R2-tests
             move-bias-tests
             allocate-registers-tests
             move-related-tests
             color-graph-tests
             live-afters-tests
             make-conflicts-tests
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
             interp-C0-tests
             interp-C1-tests
             typecheck-R2-tests
             interp-R2-tests
             interp-R1-tests
             interp-R0-tests))
