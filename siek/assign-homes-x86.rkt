#lang racket

(provide assign-homes-x86
         colors->homes)

(require "raise-mismatch-error.rkt"
         "options.rkt")

(define (assign-homes-x86 env instr*)
  (define (assign-homes-instr env i)
    (define-match-expander prim
      (lambda (stx)
        (syntax-case stx ()
          [(_ op a* ...)
           #'(list op (app (lambda (a) (assign-homes-arg env a)) a*) ...)])))

    (define (assign-homes-arg env a)
      (match a
        [`(int ,_) a]
        [`(reg ,_) a]
        ;; TODO assign-homes-x861
        [`(bytereg ,_) a]
        [`(var ,v) (dict-ref env v)]
        [_
         (raise-mismatch-error 'assign-homes 'arg a)]))

    (match i
      [`(jmp ,_) i]
      [`(je ,_) i]
      [`(jl ,_) i]
      [`(callq ,_) i]
      [(prim op a) `(,op ,a)]
      [(prim op a0 a1) `(,op ,a0 ,a1)]
      [_
       (raise-mismatch-error 'assign-homes 'instr i)]))
  (map (lambda (i) (assign-homes-instr env i)) instr*))

(define (colors->homes colors)
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

  (for/hash ([(v c) (in-hash colors)])
    (values v (color->arg c))))
