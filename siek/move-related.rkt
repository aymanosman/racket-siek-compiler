#lang racket

(provide move-related)

(require graph
         (only-in "match-instr.rkt" arg))

(define (move-related locals i*)
  (define g (undirected-graph locals))

  (for ([i i*])
    (match i
      [(list 'movq `(var ,x0) `(var ,x1))
       (when (not (equal? x0 x1))
         (add-edge! g x0 x1))]
      [(list (or 'movq 'addq 'cmpq) _ (arg a))
       (add-vertex! g a)]
      [(list (or 'negq) (arg a))
       (add-vertex! g a)]
      [(list (or 'jmp 'jl 'je) _)
       (void)]
      [`(callq ,_)
       (void)]))
  g)
