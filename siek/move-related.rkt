#lang racket

(provide move-related)

(require graph
         (only-in "match-instr.rkt" arg))

;; TODO extendable R2
(define (move-related locals i*)
  (define g (undirected-graph locals))

  (for ([i i*])
    (match i
      [(list 'movq `(var ,x0) `(var ,x1))
       (when (not (equal? x0 x1))
         (add-edge! g x0 x1))]
      [(list (or 'movq 'movzbq 'addq 'cmpq) _ (arg a))
       (add-vertex! g a)]
      [(list (or 'negq 'sete 'setl) (arg a))
       (add-vertex! g a)]
      [(list (or 'jmp 'jl 'je) _)
       (void)]
      [`(callq ,_)
       (void)]))
  g)
