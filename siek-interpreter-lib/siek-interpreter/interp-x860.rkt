#lang racket

(provide interp-x860
         interp-x860*)

(require racket/fixnum)

(define current-x86 (make-parameter 'x860))

(define (interp-x860* p)
  (parameterize ([current-x86 'x860*])
    (interp-x860 p)))

(define (interp-x860 p)
  (match p
    [`(program ,info
               ((start . ,block)))
     (lookup (interp-block block) 'rax)]
    [_
     (report-mismatch-error 'top p)]))

(define (interp-block b)
  (match b
    [`(block ,info ,instr* ...)
     (interp-instr* '() instr*)]
    [_
     (report-mismatch-error 'block b)]))

(define (interp-instr* env i*)
  (cond
    [(empty? i*)
     env]
    [else
     (interp-instr* (interp-instr env (first i*)) (rest i*))]))

(define (interp-instr env i)
  (match i
    [`(addq ,a0 ,a1)
     (extend env (cons (l-value a1)
                       (fx+ (r-value env a0) (r-value env a1))))]
    ;; subq
    [`(movq ,a0 ,a1)
     (extend env (cons (l-value a1)
                       (r-value env a0)))]
    ;; ret
    [`(negq ,a)
     (extend env (cons (l-value a) (fx- 0 (r-value env a))))]
    ;; callq
    ;; pushq
    ;; popq
    [_
     (report-mismatch-error 'instr i)]))

;; Aux

(define (report-mismatch-error kind term)
  (raise-arguments-error 'interp-x860 "failed match"
                         "kind" kind
                         "term" term))

(define (extend env entry)
  (cons entry env))

(define (lookup env loc)
  (match (assoc loc env)
    [(cons _ value) value]
    [_
     (raise-arguments-error 'interp-x860 "location read before being written"
                            "location" loc)]))

(define (l-value a)
  (match a
    [`(int ,_) (raise-arguments-error 'interp-x860 "invalid l-value"
                                      "arg" a)]
    [`(reg ,r) r]
    [`(deref ,reg ,m) m]
    [`(var ,v)
     (case (current-x86)
       [(x860*) v]
       [else (report-variables-not-supported-error v)])]
    [_
     (report-mismatch-error 'l-value a)]))

(define (r-value env a)
  (match a
    [`(int ,n) n]
    [`(reg ,r) (lookup env r)]
    [`(var ,v)
     (case (current-x86)
       [(x860*) (lookup env v)]
       [else (report-variables-not-supported-error v)])]
    [`(deref ,reg ,m) (lookup env m)]
    [_
     (report-mismatch-error 'r-value a)]))

(define (report-variables-not-supported-error v)
  (raise-arguments-error 'interp-x860 "variables are not supported in the current language (did you mean to use pseudo-x86?)"
                         "current-x86" (current-x86)
                         "variable" v))
