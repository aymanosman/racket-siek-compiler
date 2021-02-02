#lang racket

(provide live-afters
         live-afters-instr*)

(require graph
         (only-in "match-instr.rkt" arg)
         "block.rkt"
         "liveness.rkt"
         "raise-mismatch-error.rkt")

(define (live-afters code)
  (define live-env (make-hash))
  (for ([l (rest (reverse-toplogical-order code))]) ;; skip conclusion
    (define live-set
      (live-afters-instr* live-env (block-instr* (dict-ref code l))))
    (hash-set! live-env l live-set))
  (for/hash ([(k v) (in-hash live-env)])
    (values k (rest v))))

(define (reverse-toplogical-order code)
  (tsort
   (transpose
    (directed-graph
     (append*
      (for/lists (_) ([c code])
        (match-define (cons label `(block ,_ ,instr* ...)) c)
        (instr*->edges label instr*)))))))

(define (instr->label i)
  (match i
    [`(jmp ,l) l]
    [`(je ,l) l]
    [`(jl ,l) l]
    [_ #f]))

(define (instr*->edges label instr*)
  (for/list ([l (stream-filter identity
                               (for/stream ([i instr*]) (instr->label i)))])
    (list label l)))

(define live-afters-instr*
  (case-lambda
   [(live-env instr*)
    (live-afters-instr* (list (set)) live-env (reverse instr*))]
   ;; L(k) = L(k+1) - W(k) + R(k)
   [(acc live-env i*)
    (match i*
      ['() acc]
      [(cons i i*)
       (define next
         (set-union (set-subtract (first acc) (instr->writes i))
                    (instr->reads live-env i)))
       (live-afters-instr* (cons next acc) live-env i*)])]))
