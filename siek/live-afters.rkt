#lang racket

(provide live-afters
         live-afters-instr*
         label->live)

(require graph
         (only-in "match-instr.rkt" arg)
         "block.rkt"
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
         (set-union (set-subtract (first acc) (instr->writes live-env i))
                    (instr->reads live-env i)))
       (live-afters-instr* (cons next acc) live-env i*)])]))

(define-match-expander instr
  (lambda (stx)
    (syntax-case stx ()
      [(_ op p q)
       #'(list op p q)])))

;; (: instr->reads (-> Env Any (Setof Symbol)))
(define (instr->reads env i)
  (match i
    [`(negq ,(arg a)) (set a)]
    [(instr (or 'addq 'cmpq) (arg a0) (arg a1)) (set a0 a1)]
    [(instr (or 'addq 'cmpq) _ (arg a)) (set a)]
    [`(movq ,(arg a) ,_) (set a)]
    [`(movq ,_ ,_) (set)]
    [`(jmp ,l) (label->live env l)]
    [`(je ,l) (label->live env l)]
    [`(jl ,l) (label->live env l)]
    [`(callq ,l) (set)]
    [_ (raise-mismatch-error 'instr->reads 'instr i)]))

;; (: instr->writes (-> Env Any (Setof Symbol)))
(define (instr->writes env i)
  (match i
    [`(negq ,(arg a)) (set a)]
    [`(addq ,_ ,(arg a)) (set a)]
    [`(movq ,_ ,(arg a)) (set a)]
    [`(cmpq ,_ ,_) (set)] ;; TODO
    [`(jmp ,_) (set)]
    [`(je ,_) (set)]
    [`(jl ,_) (set)]
    [`(callq ,_) (set)]
    [_ (raise-mismatch-error 'instr->writes 'instr i)]))

;; (: label->live (-> Env Symbol (Setof Symbol)))
(define (label->live env l)
  (match l
    ['conclusion (set 'rax 'rsp)]
    [_
     (first (dict-ref env l))]))
