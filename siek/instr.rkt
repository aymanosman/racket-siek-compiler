#lang racket

(provide instr->reads
         instr->writes)

(require "match-instr.rkt")

(define (instr->reads i)
  (match i
    [`(negq ,(arg a)) (set a)]
    [`(addq ,(arg a0) ,(arg a1)) (set a0 a1)]
    [`(addq ,_ ,(arg a1)) (set a1)]
    [`(movq ,(arg a) ,_) (set a)]
    [`(movq ,_ ,_) (set)]
    [`(jmp ,_) (set)]
    [`(,_) (set)]
    [_ (report-mismatch-error 'instr->reads 'instr i)]))

(define (instr->writes i)
  (match i
    [`(negq ,(arg a)) (set a)]
    [`(addq ,_ ,(arg a)) (set a)]
    [`(movq ,_ ,(arg a)) (set a)]
    [`(jmp ,_) (set)]
    [`(,_) (set)]
    [_ (report-mismatch-error 'instr->writes 'instr i)]))

;; Aux

(define (report-mismatch-error who kind term)
  (raise-arguments-error who
                         "failed match"
                         "kind"
                         kind
                         "term"
                         term))
