#lang racket

(provide assign-homes
         colors->homes)

(require "raise-mismatch-error.rkt"
         "options.rkt")

(define (assign-homes env instr*)
  (map (lambda (i) (assign-homes-instr env i)) instr*))

(define (assign-homes-instr env i)
  (match i
    [`(jmp ,l) i]
    [`(callq ,l) i]
    [`(,op ,a* ...)
     `(,op ,@(map (lambda (a) (assign-homes-arg env a)) a*))]
    [_
     (raise-mismatch-error 'assign-homes 'instr i)]))

(define (assign-homes-arg env a)
  (match a
    [`(int ,_) a]
    [`(reg ,_) a]
    [`(var ,v) (dict-ref env v)]
    [_
     (raise-mismatch-error 'assign-homes 'arg a)]))

(define (colors->homes colors)
  (for/hash ([(v c) (in-hash colors)])
    (values v (color->arg c))))

(define (color->arg c)
  (cond
    [(and (>= c 0) (< c (compiler-stack-location-index)))
     `(reg ,(hash-ref register-table c))]
    [else
     `(deref rbp ,(stack-offset (- c (compiler-stack-location-index))))]))

(define (stack-offset n)
  (- (* 8 (add1 n))))

(define register-table
  (hash 0 'rbx
        1 'rcx
        2 'rdx))
