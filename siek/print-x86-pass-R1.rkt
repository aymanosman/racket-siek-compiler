#lang racket

(provide print-x86-pass-R1)

(require "options.rkt")

(define (print-x86-pass-R1 p)
  (match p
    [`(program ,info ((start . ,block)))
     (print-x86-label 'start)
     (printf ":\n")
     (print-x86-block block)

     (print-x86-main)
     (print-x86-conclusion)]
    [_ (report-mismatch-error 'top p)]))

(define (print-x86-block b)
  (match b
    [`(block ,info ,instr* ...)
     (for-each (lambda (i)
                 (display "  ")
                 (print-x86-instr i)
                 (newline))
               instr*)]
    [_ (report-mismatch-error 'block b)]))

(define (print-x86-instr i)
  (match i
    [`(retq)
     (printf "retq")]
    [`(,op ,a)
     (printf "~a " op)
     (print-x86-arg a)]
    [`(,op ,a0 ,a1)
     (printf "~a " op)
     (print-x86-arg a0)
     (printf ", ")
     (print-x86-arg a1)]
    [_ (report-mismatch-error 'instr i)]))

(define (print-x86-arg a)
  (match a
    [(? symbol?)
     (print-x86-label a)]
    [`(int ,n)
     (printf "$~a" n)]
    [`(reg ,r)
     (printf "%~a" r)]
    [`(deref ,r ,n)
     (printf "~a(%~a)" n r)]
    [_ (report-mismatch-error 'arg a)]))

(define (print-x86-label l)
  (case (current-system-type)
    [(macosx)
     (printf "_~a" l)]
    [else
     (printf "~a" l)]))

(define (print-x86-main)
  (printf "  .global ")
  (print-x86-label 'main)
  (newline)
  (print-x86-label 'main)
  (printf ":\n")
  (print-x86-block
   '(block ()
           (pushq (reg rbp))
           (movq (reg rsp) (reg rbp))
           (subq (int 16) (reg rsp))
           (jmp start))))

(define (print-x86-conclusion)
  (print-x86-label 'conclusion)
  (printf ":\n")
  (print-x86-block
   '(block ()
           (addq (int 16) (reg rsp))
           (popq (reg rbp))
           (retq))))

;; Aux

(define (report-mismatch-error kind term)
  (raise-arguments-error 'print-x86-pass-R1 "failed match"
                         "kind" kind
                         "term" term))
