#lang racket

(provide instr->writes
         instr->reads)

(require "raise-mismatch-error.rkt"
         (only-in "match-instr.rkt" arg))

;; (: instr->writes (-> Any (Setof Symbol)))
(define (instr->writes i)
  (local-require racket/set)
  (match i
    [(list 'negq (arg a)) (set a)]
    [(list (or 'sete 'setl) (arg a)) (set)]
    [(list (or 'addq 'movq 'cmpq 'movzbq) _ (arg a)) (set a)]
    [(list (or 'callq 'jmp 'jl 'je) _) (set)]
    [_ (raise-mismatch-error 'instr->writes 'instr i)]))

;; (: instr->reads (-> Env Any (Setof Symbol)))
(define (instr->reads env i)
  (match i
    [(list (or 'negq 'sete 'setl) (arg a)) (set a)]
    [(list (or 'addq 'cmpq) (arg a0) (arg a1)) (set a0 a1)]
    [(list (or 'addq 'cmpq) _ (arg a)) (set a)]
    [(list (or 'movq 'movzbq) (arg a) _) (set a)]
    [(list (or 'movq 'movzbq) _ _) (set)]
    [`(jmp ,l) (label->live env l)]
    [`(je ,l) (label->live env l)]
    [`(jl ,l) (label->live env l)]
    [`(callq ,l) (set)]
    [_ (raise-mismatch-error 'instr->reads 'instr i)]))

;; (: label->live (-> Env Symbol (Setof Symbol)))
(define (label->live env l)
  (match l
    ['conclusion (set 'rax 'rsp)]
    [_
     (first (dict-ref env l))]))
