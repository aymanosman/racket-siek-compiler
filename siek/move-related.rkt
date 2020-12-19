#lang racket

(provide move-related)

(require graph
         "match-instr.rkt")

(define (move-related i*)
  (define g (undirected-graph '()))

  (for ([i i*])
    (match i
      [`(movq (var ,a0) (var ,a1))
       (add-edge! g a0 a1)]
      [`(,_ ,(arg a))
       (add-vertex! g a)]
      [`(,op ,(arg a0) ,(arg a1))
       (add-vertex! g a0)
       (add-vertex! g a1)]
      [_ (void)]))

  g)
