#lang racket

(provide x860?
         x860*?)

(require "raise-mismatch-error.rkt"
         "options.rkt")

(define (x860? p)
  (match p
    [`(program ,_ ((start . ,block)))
     (x860-block? block)]
    [_ (raise-mismatch-error (current-x86) 'top p)]))

(define (x860*? p)
  (parameterize ([current-x86 'x860*])
    (x860? p)))

(define (x860-block? b)
  (match b
    [`(block ,_ ,instr* ...)
     (andmap x860*-instr? instr*)]
    [_ (raise-mismatch-error (current-x86) 'block b)]))

(define (x860*-instr? i)
  (match i
    [`(movq ,a0 ,a1) (x860-args? a0 a1)]
    [`(negq ,a) (x860-args? a)]
    [`(addq ,a0 ,a1) (x860-args? a0 a1)]
    [`(subq ,a0 ,a1) (x860-args? a0 a1)]
    [`(retq) #t]
    [`(callq ,label)
     (cond
       [(symbol? label) #t]
       [else (raise-mismatch-error (current-x86) 'label label)])]
    [`(pushq ,a) (x860-args? a)]
    [`(popq ,a) (x860-args? a)]
    [_ (raise-mismatch-error (current-x86) 'instr i)]))

(define (x860-arg? a)
  (match a
    [`(int ,n)
     (cond
       [(fixnum? n) #t]
       [else (raise-mismatch-error (current-x86) 'int n)])]
    [`(reg ,r)
     (cond
       [(x860-register? r) #t]
       [else (raise-mismatch-error (current-x86) 'register r)])]
    [`(deref ,r ,n)
     (define errors
       (filter identity
               (list
                (cond
                  [(x860-register? r) #f]
                  [else `(register ,r)])
                (cond
                  [(fixnum? n) #f]
                  [else `(int ,n)]))))

     (cond
       [(empty? errors) #t]
       [else
        (raise-mismatch-error (current-x86) 'errors errors)])]
    [`(var ,v)
     (cond
       [(equal? (current-x86) 'x860*)
        (cond
          [(symbol? v) #t]
          [else
           (raise-mismatch-error (current-x86) 'var v)])]
       [else
        (raise-mismatch-error (current-x86)
                              'errors (list (cons 'arg a)
                                               (cons 'variables-not-supported a)))])]

    [_ (raise-mismatch-error (current-x86) 'arg a)]))

(define (x860-register? r)
  (cond
    [(member r '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
     #t]
    [else #f]))

(define (x860-args? . a*)
  (define errors
    (filter identity
            (map (lambda (a)
                   (cond
                     [(x860-arg? a) #f]
                     [else `(arg ,a)]))
                 a*)))
  (cond
    [(empty? errors) #t]
    [else
     (raise-mismatch-error (current-x86) 'errors errors)]))
