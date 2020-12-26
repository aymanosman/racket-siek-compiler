#lang racket

(provide build-interference)

(require "instr.rkt"
         "match-instr.rkt"
         graph)

(define build-interference
  (case-lambda
   [(l* i*)
    (build-interference (unweighted-graph/undirected '()) l* i*)]
   [(g l* i*)
    (for ([l l*]
          [i i*])
      (match i
        [`(movq ,(arg src) ,(arg dest))
         (for ([v l])
           (when (not (or (symbol=? v src)
                          (symbol=? v dest)))
                 (add-edge! g dest v)))]
        [_
         (for* ([dest (instr->writes i)]
                [v l])
           (when (not (symbol=? v dest))
                 (add-edge! g dest v)))]))
    g]))

(module+ test
  (require rackunit)

  (require "live-afters.rkt")

  (let ()
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
