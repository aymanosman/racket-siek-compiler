#lang racket

(provide move-related-tests)

(require rackunit)

(require graph)

(require siek/move-related)

(define-test-suite move-related-tests
  (test-case "no move related variables"
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
        (jmp conclusion)))

    (define moves (move-related instr*))

    (check-equal? (length (get-edges moves)) 0))

  (test-case "some move related variables"
    (define instr*
      '((movq (int 1) (var v))
        (movq (int 46) (var w))
        (movq (var v) (var x)) ;; v x
        (addq (int 7) (var x))
        (movq (var x) (var y)) ;; x y
        (movq (var x) (var z)) ;; x z
        (addq (var w) (var z))
        (movq (var y) (var t.1)) ;; y t.1
        (negq (var t.1))
        (movq (var z) (var t.2)) ;; z t.2
        (addq (var t.1) (var t.2))
        (movq (var t.2) (reg rax))
        (jmp conclusion)))

    (define moves (move-related instr*))

    (check-equal? (length (get-edges moves)) (* 5 2))))