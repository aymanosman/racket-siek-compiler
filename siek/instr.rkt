#lang racket

(provide instr->reads
         instr->writes)

(require "match-instr.rkt"
         "raise-mismatch-error.rkt")

(define (instr->reads i)
  (match i
    [`(negq ,(arg a)) (set a)]
    [`(addq ,(arg a0) ,(arg a1)) (set a0 a1)]
    [`(addq ,_ ,(arg a1)) (set a1)]
    [`(movq ,(arg a) ,_) (set a)]
    [`(movq ,_ ,_) (set)]
    [`(jmp ,_) (set)]
    [`(callq ,_) (set)]
    [_ (raise-mismatch-error 'instr->reads 'instr i)]))

(define (instr->writes i)
  (match i
    [`(negq ,(arg a)) (set a)]
    [`(addq ,_ ,(arg a)) (set a)]
    [`(movq ,_ ,(arg a)) (set a)]
    [`(jmp ,_) (set)]
    [`(callq ,_) (set)]
    [_ (raise-mismatch-error 'instr->writes 'instr i)]))
