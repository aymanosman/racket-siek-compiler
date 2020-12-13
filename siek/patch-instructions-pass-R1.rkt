#lang racket

(provide patch-instructions-pass-R1)

(require "raise-mismatch-error.rkt")

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
    [`(,op ,a) (list i)]
    [`(,op ,a0 ,a1)
     (cond
       [(and (deref? a0) (deref? a1))
        `((movq ,a0 (reg rax))
          (,op (reg rax) ,a1))]
       [else (list i)])]
    [_ (raise-mismatch-error 'patch-instructions-pass-R1 'instr i)]))

;; Aux

(define (deref? a)
  (match a
    [`(deref ,_ ,_) #t]
    [_ #f]))
