#lang racket

(provide move-bias-tests)

(require rackunit
         siek/color-graph
         siek/live-afters
         siek/build-interference
         siek/assign-homes
         siek/move-related
         (submod siek/patch-instructions-pass-R1 for-test))

(module+ test
  (require rackunit/text-ui)
  (run-tests move-bias-tests))

(define-check (check-optimized input opt norm)
  (define num-inst
    (list (make-check-info 'input (length input))
          (make-check-info 'optimized (length opt))
          (make-check-info 'normal (length norm))))
  (with-check-info (['params '()]
                    ['number-of-instructions (nested-info num-inst)]
                    ['optimized opt]
                    ['normal norm])
                   (unless (< (length opt) (length norm))
                     (fail-check))))

(define-test-suite move-bias-tests
  (test-case "running example"
    (define instr*
      '((movq (int 1) (var v))
        (movq (int 42) (var w))
        (movq (var v) (var x))
        (addq (int 7) (var x))
        (movq (var x) (var y))
        (movq (var x) (var z))
        (addq (var w) (var z))
        (movq (var y) (var t))
        (negq (var t))
        (movq (var z) (reg rax))
        (addq (var t) (reg rax))
        (jmp conclusion)))

    (define live* (live-afters instr*))
    (define conflict (build-interference live* instr*))
    (define moves (move-related instr*))
    (define (process moves order)
      (define coloring (color-graph conflict moves #:order-for-test order))
      (append-map patch-instructions-instr (assign-homes (colors->homes coloring) instr*)))

    (check-optimized instr*
                     (process moves #f)
                     (process #f '(t z w y v x)))))
