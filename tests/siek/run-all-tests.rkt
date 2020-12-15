#lang racket

(require rackunit
         rackunit/text-ui
         "x860.rkt"
         "patch-instructions-pass-R1.rkt"
         "assign-homes-pass-R1.rkt"
         "select-instructions-pass-R1.rkt"
         "uncover-locals-pass-R1.rkt"
         "explicate-control-pass-R1.rkt"
         "remove-complex-opera-pass-R1.rkt"
         "uniquify-pass-R1.rkt"
         "interp-x860.rkt")

(run-tests
 (test-suite "all-tests"
             x860-tests
             patch-instructions-tests
             assign-homes-tests
             select-instructions-tests
             uncover-locals-tests
             explicate-control-tests
             remove-complex-opera*-tests
             uniquify-tests
             interp-x860-tests))
