#lang racket

(provide build-interference-tests)

(require rackunit)

(require siek)

(require graph)

(define-test-suite build-interference-tests
  (test-case "running example"
    (check-equal?
     (build-interference-pass-R1
      `(program
        ()
        ((start .
                (block ((live-afters .
                                     ,(list
                                       (set 'v)
                                       (set 'v 'w)
                                       (set 'w 'x)
                                       (set 'w 'x)
                                       (set 'w 'x 'y)
                                       (set 'w 'y 'z)
                                       (set 'y 'z)
                                       (set 'z 't.1)
                                       (set 'z 't.1)
                                       (set 't.1 't.2)
                                       (set 't.2)
                                       (set)
                                       (set))))
                       (movq (int 1) (var v))
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
                       (jmp conclusion))))))
     `(program
       ()
       ((start .
               (block ((conflicts .
                                  ,(unweighted-graph/undirected
                                    '((w v)
                                      (x w)
                                      (x w)
                                      (y w)
                                      (z w)
                                      (z y)
                                      (z w)
                                      (z y)
                                      (t.1 z)
                                      (t.1 z)
                                      (t.2 t.1)
                                      (t.2 t.1)))))
                      (movq (int 1) (var v))
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
                      (jmp conclusion))))))))
