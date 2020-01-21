#lang racket

(provide patch-instructions-pass-R1)

(define (patch-instructions-pass-R1 p)
  (match p
    [`(program ,info ((start . ,block)))
     `(program ,info ((start . ,(patch-instructions-block block))))]
    [_ (report-mismatch-error 'top p)]))

(define (patch-instructions-block b)
  (match b
    [`(block ,info ,instr* ...)
     `(block ,info ,@(append-map patch-instructions-instr instr*))]
    [_ (report-mismatch-error 'block b)]))

(define (patch-instructions-instr i)
  (match i
    [`(,op ,a) (list i)]
    [`(,op ,a0 ,a1)
     (cond
       [(and (deref? a0) (deref? a1))
        `((movq ,a0 (reg rax))
          (,op (reg rax) ,a1))]
       [else (list i)])]
    [_ (report-mismatch-error 'instr i)]))

;; Aux

(define (deref? a)
  (match a
    [`(deref ,_ ,_) #t]
    [_ #f]))

(define (report-mismatch-error kind term)
  (raise-arguments-error 'patch-instructions-pass-R1 "failed match"
                         "kind" kind
                         "term" term))
