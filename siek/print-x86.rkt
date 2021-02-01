#lang racket

(provide print-x86-R1
         (rename-out [print-x86-R1 print-x86]))

(require "options.rkt"
         "raise-mismatch-error.rkt")

(define (print-x86-R1 p)
  (match p
    [`(program ,info ,code)
     (for ([label+block code])
       (match-define (cons label block) label+block)
       (print-x86-label label)
       (printf ":\n")
       (print-x86-block block))
     (print-x86-main)
     (print-x86-conclusion)]
    [_ (raise-mismatch-error 'print-x86-R1 'top p)]))

(define (print-x86-block b)
  (match b
    [`(block ,info ,instr* ...)
     (for-each (lambda (i)
                 (display "  ")
                 (print-x86-instr i)
                 (newline))
               instr*)]
    [_ (raise-mismatch-error 'print-x86-R1 'block b)]))

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
    [_ (raise-mismatch-error 'print-x86-R1 'instr i)]))

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
    [_ (raise-mismatch-error 'print-x86-R1 'arg a)]))

(define (print-x86-label l)
  (case (current-system-type)
    [(macosx)
     (printf "_~a" l)]
    [else
     (printf "~a" l)]))

(define (print-x86-main)
  (printf ".global ")
  (print-x86-label 'main)
  (newline)
  (print-x86-label 'main)
  (printf ":\n")
  (print-x86-block
   '(block
     ()
     (pushq (reg rbp))
     (movq (reg rsp) (reg rbp))
     (subq (int 16) (reg rsp))
     (jmp start))))

(define (print-x86-conclusion)
  (print-x86-label 'conclusion)
  (printf ":\n")
  (print-x86-block
   '(block
     ()
     (addq (int 16) (reg rsp))
     (popq (reg rbp))
     (retq))))
