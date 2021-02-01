#lang racket

(provide color-graph-tests)

(require graph
         rackunit
         siek/x86
         siek/options
         siek/color-graph
         siek/live-afters
         siek/make-conflicts
         siek/move-related)

(module+ test
  (require rackunit/text-ui)
  (run-tests color-graph-tests))

(define-check (check-valid-coloring conflict coloring)
  (define g (graph-copy conflict))
  (remove-vertex! g 'rax)
  (remove-vertex! g 'rsp)
  (with-check-info (['coloring (unquoted-printing-string (graphviz g #:colors coloring))])
    (check-true (valid-coloring? g coloring))))

(define-check (check-num-colors coloring expected)
  (define copy (hash-copy coloring))
  (for ([k (in-list (hash-keys copy))])
    (when (register? k)
      (hash-remove! copy k)))
  (define actual (length (remove-duplicates (dict-values copy))))
  (unless (equal? actual expected)
    (with-check-info (['coloring coloring]
                      ['copy copy]
                      ['actual actual]
                      ['expected expected])
      (fail-check))))

(define-test-suite color-graph-tests
  (test-case "(read)"
    (define instr*
      '((movq (int 41) (var v))
        (callq read_int)
        (addq (var v) (reg rax))
        (jmp conclusion)))

    (define live* (live-afters-instr* empty instr*))
    (define conflict (make-conflicts '(v) live* instr*))
    (define coloring
      (parameterize ([compiler-enable-move-biasing? #f])
        (color-graph conflict '(v))))

    (check-valid-coloring conflict coloring)
    (check-num-colors coloring 1))

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

    (define live* (live-afters-instr* empty instr*))
    (define locals '(u v w x y z))
    (define conflict (make-conflicts locals live* instr*))
    (define coloring (color-graph conflict locals (move-related locals instr*)))

    (check-valid-coloring conflict coloring)
    (check-num-colors coloring 6))

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

    (define live* (live-afters-instr* empty instr*))
    (define locals '(v w x y z t))
    (define conflict (make-conflicts locals live* instr*))
    (define coloring (color-graph conflict locals (move-related locals instr*)))

    (check-valid-coloring conflict coloring)
    (check-num-colors coloring 3)))
