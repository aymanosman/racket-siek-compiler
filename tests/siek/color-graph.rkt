#lang racket

(provide color-graph-tests)

(require rackunit)

(require graph)

(require siek/color-graph
         siek/live-afters
         siek/move-related
         siek/build-interference)

(define-check (check-valid-coloring conflict coloring)
  (define g (graph-copy conflict))
  ;; FIXME (for ([r registers]) remove)
  (remove-vertex! g 'rax)
  (remove-vertex! g 'rsp)
  (with-check-info (['coloring (unquoted-printing-string (graphviz g #:colors coloring))])
    (check-true (valid-coloring? g coloring))))

(define-test-suite color-graph-tests
  (test-case "fully conflicted"
    (define instr*
      '((movq (int 1) (var u))
        (movq (int 1) (var v))
        (movq (int 1) (var w))
        (movq (int 1) (var x))
        (addq (int 1) (var y))
        (addq (int 1) (var z))
        (addq (var u) (var z))
        (addq (var v) (var z))
        (addq (var w) (var z))
        (addq (var x) (var z))
        (addq (var y) (var z))
        (movq (var z) (reg rax))
        (jmp conclusion)))

    (define live* (live-afters instr*))
    (define conflict (build-interference live* instr*))
    (define coloring (color-graph conflict (move-related instr*)))
    (define num-colors (length (remove-duplicates (dict-values coloring))))

    (check-valid-coloring conflict coloring)
    (check-equal? num-colors 6))

  ;; FIXME fix test
  #;
  (test-case "running example"
    (define instr*
      '((movq (int 1) (var v))
        (movq (int 46) (var w))
        (movq (var v) (var x))
        (addq (int 7) (var x))
        (movq (var x) (var y))
        (movq (var x) (var z))
        (addq (var w) (var z))
        (movq (var y) (var t.1))
        (negq (var t.1))
        (movq (var z) (var t.2))
        (addq (var t.1) (var t.2))
        (movq (var t.2) (reg rax))
        (jmp conclusion)))

    (define live* (live-afters instr*))
    (define conflict (build-interference live* instr*))
    (define coloring (color-graph conflict (move-related instr*)))
    (define num-colors (length (remove-duplicates (dict-values coloring))))

    (check-valid-coloring conflict coloring)
    (check-equal? num-colors 3))

  (test-case "other running example"
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
    (define coloring (color-graph conflict (move-related instr*)))
    (define num-colors (length (remove-duplicates (dict-values coloring))))

    (check-valid-coloring conflict coloring)
    (check-equal? num-colors 3)))
