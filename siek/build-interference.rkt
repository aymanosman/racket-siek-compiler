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
         (for ([v l]
               #:when
               (not (or (symbol=? v src) (symbol=? v dest))))
           (add-edge! g dest v))]
        ;; TODO handle callq
        [_
         (for* ([dest (instr->writes i)]
                [v l])
           (when (not (symbol=? v dest))
                 (add-edge! g dest v)))]))
    g]))
