#lang racket

(provide build-interference-tests)

(require rackunit)

(require graph
         siek/live-afters
         siek/build-interference)

(module+ test
  (require rackunit/text-ui)
  (run-tests build-interference-tests))

(define-test-suite build-interference-tests
  #;
  (test-case "callq and caller-saved registers"
    (define instr*
      '((movq (int 1) (var v))   ;; v = 1
        (callq 0 (read_int))     ;; w = (read)
        (movq (reg rax) (var w))
        (addq (var v) (var w))   ;; w = w +v
        (movq (var w) (reg rax)) ;; return w
        (jmp conclusion)))

    (define l* (live-afters instr*))
    (define g (build-interference l* instr*))

    (check-equal? (list->set (get-neighbors g 'v))
                  (list->set caller-saved-registers)))

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
    (define l* (live-afters instr*))
    (define g (build-interference l* instr*))
    (define g2
      (let ()
        (define g (unweighted-graph/undirected '()))
        (for ([n '(v w x y z t rax)])
          (add-edge! g n 'rsp))
        (add-edge! g 'w 'v)
        (add-edge! g 'x 'w)
        (add-edge! g 'y 'w)
        (add-edge! g 'z 'w)
        (add-edge! g 'z 'y)
        (add-edge! g 'z 'y)
        (add-edge! g 't 'z)
        (add-edge! g 't 'rax)
        g))
    (check-equal? g g2)))
