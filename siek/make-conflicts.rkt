#lang racket

(provide make-conflicts)

(require graph
         "x86.rkt"
         "raise-mismatch-error.rkt"
         "match-instr.rkt")

(define (make-conflicts locals l* i*)
  (make-conflicts-aux (unweighted-graph/undirected locals) l* i*))

(define (make-conflicts-aux g l* i*)
  (for ([l l*]
        [i i*])
    (match i
      [`(movq ,(arg src) ,(arg dest))
       (for ([v l]
             #:when
             (not (or (symbol=? v src) (symbol=? v dest))))
         (add-edge! g dest v))]
      [`(callq ,_)
       (for* ([r (caller-saved-registers)]
              [x l])
         (add-edge! g x r))]
      [_
       (for* ([dest (instr->writes i)]
              [v l])
         (when (not (symbol=? v dest))
           (add-edge! g dest v)))]))
  g)

;; TODO refactor
;; (: instr->writes (-> Any (Setof Symbol)))
(define (instr->writes i)
  (local-require racket/set)
  (match i
    [(list 'negq (arg a)) (set a)]
    [(list (or 'addq 'movq 'cmpq) _ (arg a)) (set a)]
    [(list (or 'callq 'jmp 'jl 'je) _) (set)]
    [_ (raise-mismatch-error 'instr->writes 'instr i)]))
