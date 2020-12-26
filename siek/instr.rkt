#lang racket

(provide instr->reads
         instr->writes)

(require (only-in "match-instr.rkt" arg)
         "raise-mismatch-error.rkt")

(define (instr->reads i)
  (match i
    [`(negq ,(arg a)) (set a)]
    [`(addq ,(arg a0) ,(arg a1)) (set a0 a1)]
    [`(addq ,_ ,(arg a1)) (set a1)]
    [`(movq ,(arg a) ,_) (set a)]
    [`(movq ,_ ,_) (set)]
    [`(jmp ,l) (label->live l)]
    [`(callq ,n ,l)
     (set)]
    [_ (raise-mismatch-error 'instr->reads 'instr i)]))

(define (instr->writes i)
  (match i
    [`(negq ,(arg a)) (set a)]
    [`(addq ,_ ,(arg a)) (set a)]
    [`(movq ,_ ,(arg a)) (set a)]
    [`(jmp ,_) (set)]
    [`(callq ,n ,_)
     (set)]
    [_ (raise-mismatch-error 'instr->writes 'instr i)]))

(define (label->live l)
  (match l
    ['conclusion (set 'rax 'rsp)]))
