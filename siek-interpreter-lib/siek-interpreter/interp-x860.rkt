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
     (raise-arguments-error 'interp-x860 "failed match"
                            "kind" 'top
                            "term" p)]))

(define (interp-block b)
  (match b
    [`(block ,info ,instr* ...)
     (interp-instr* '() instr*)]
    [_
     (raise-arguments-error 'interp-x860 "failed match"
                            "kind" 'block
                            "term" b)]))

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
     (raise-arguments-error 'interp-x860 "failed match"
                            "kind" 'instr
                            "term" i)]))

;; Aux

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
     (raise-arguments-error 'interp-x860 "failed match"
                            "kind" 'l-value
                            "term" a)]))

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
     (raise-arguments-error 'interp-x860 "failed match"
                            "kind" 'r-value
                            "term" a)]))

(define (report-variables-not-supported-error v)
  (raise-arguments-error 'interp-x860 "variables are not supported in the current language (did you mean to use pseudo-x86?)"
                         "current-x86" (current-x86)
                         "variable" v))
