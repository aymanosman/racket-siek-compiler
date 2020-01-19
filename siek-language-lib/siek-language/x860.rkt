#lang racket

(provide x860?
         x860*?
         current-x860-mismatch-handler)

(define (x860? p)
  (match p
    [`(program ,info ((start . ,block)))
     (x860-block? block)]
    [_ ((current-x860-mismatch-handler) 'top p)]))

(define (x860*? p)
  (parameterize ([current-x86 'x860*])
    (x860? p)))

(define (x860-block? b)
  (match b
    [`(block ,info ,instr* ...)
     (andmap x860*-instr? instr*)]
    [_ ((current-x860-mismatch-handler) 'block b)]))

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
       [else ((current-x860-mismatch-handler) 'label label)])]
    [`(pushq ,a) (x860-args? a)]
    [`(popq ,a) (x860-args? a)]
    [_ ((current-x860-mismatch-handler) 'instr i)]))

(define (x860-arg? a)
  (match a
    [`(int ,n)
     (cond
       [(fixnum? n) #t]
       [else ((current-x860-mismatch-handler) 'int n)])]
    [`(reg ,r)
     (cond
       [(x860-register? r) #t]
       [else ((current-x860-mismatch-handler) 'register r)])]
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
        ((current-x860-mismatch-handler) errors)])]
    [`(var ,v)
     (cond
       [(equal? (current-x86) 'x860*)
        (cond
          [(symbol? v) #t]
          [else
           ((current-x860-mismatch-handler) 'var v)])]
       [else
        ((current-x860-mismatch-handler) (list (cons 'arg a)
                                               (cons 'variables-not-supported a)))])]

    [_ ((current-x860-mismatch-handler) 'arg a)]))

(define (x860-register? r)
  (cond
    [(member r '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
     #t]
    [else #f]))

;; Aux

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
     ((current-x860-mismatch-handler) errors)]))

(define current-x86 (make-parameter 'x860))

(define current-x860-mismatch-handler
  (make-parameter
   (case-lambda
     [(kind term) #f]
     [(errors) #f])))
