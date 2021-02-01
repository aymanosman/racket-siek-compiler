#lang racket

(provide make-conflicts-tests)

(require rackunit)

(require graph
         siek/live-afters
         siek/x86
         siek/make-conflicts)

(module+ test
  (require rackunit/text-ui)
  (run-tests make-conflicts-tests))

(define-check (check-subset? smaller larger)
  (unless (subset? smaller larger)
    (with-check-info (['smaller smaller]
                      ['larger larger])
      (fail-check))))

(define-test-suite make-conflicts-tests
  (test-case "callq and caller-saved registers"
    (define instr*
      '((movq (int 1) (var v))   ;; v = 1
        (callq (read_int))       ;; w = (read)
        (movq (reg rax) (var w))
        (addq (var v) (var w))   ;; w = w + v
        (movq (var w) (reg rax)) ;; return w
        (jmp conclusion)))

    (define l* (live-afters-instr* empty instr*))
    (define g (make-conflicts '(v w) l* instr*))

    (check-subset? (caller-saved-registers) (list->set (get-neighbors g 'v))))

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
    (define l* (live-afters-instr* empty instr*))
    (define g (make-conflicts '(v w x y z t) l* instr*))
    (define g2
      (unweighted-graph/undirected (append
                                    (for/list ([n '(v w x y z t rax)]) (list n 'rsp))
                                    (list (list 'w 'v)
                                          (list 'x 'w)
                                          (list 'y 'w)
                                          (list 'z 'w)
                                          (list 'z 'y)
                                          (list 'z 'y)
                                          (list 't 'z)
                                          (list 't 'rax)))))
    (check-equal? g g2)))
