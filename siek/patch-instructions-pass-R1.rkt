#lang racket

(provide patch-instructions-pass-R1)

(require "match-instr.rkt"
         "raise-mismatch-error.rkt")

(define (patch-instructions-pass-R1 p)
  (match p
    [`(program ,info ((start . ,block)))
     `(program ,info ((start . ,(patch-instructions-block block))))]
    [_ (raise-mismatch-error 'patch-instructions-pass-R1 'top p)]))

(define (patch-instructions-block b)
  (match b
    [`(block ,info ,instr* ...)
     `(block ,info ,@(append-map patch-instructions-instr instr*))]
    [_ (raise-mismatch-error 'patch-instructions-pass-R1 'block b)]))

(define (patch-instructions-instr i)
  (match i
    [`(,op ,a)
     (list i)]
    [`(,op (deref ,a0) (deref ,a1))
     `((movq ,a0 (reg rax))
       (,op (reg rax) ,a1))]
    [`(movq ,(arg a0) ,(arg a1)) #:when (symbol=? a0 a1)
     empty]
    [`(,op ,a0 ,a1)
     (list i)]
    [_
     (raise-mismatch-error 'patch-instructions-pass-R1 'instr i)]))
