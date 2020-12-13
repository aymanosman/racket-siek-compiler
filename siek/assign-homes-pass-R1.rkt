#lang racket

(provide assign-homes-pass-R1)

(require "raise-mismatch-error.rkt")

(define (assign-homes-pass-R1 p)
  (match p
    [`(program ,info ((start . ,block)))
     (define-values (env stack-space) (locals->homes (dict-ref info 'locals)))
     `(program
       ((stack-space . ,stack-space))
       ((start . ,(assign-homes-block env block))))]
    [_
     (raise-mismatch-error 'assign-homes-pass-R1 'top p)]))

(define (assign-homes-block env b)
  (match b
    [`(block ,info ,instr* ...)
     `(block ,info ,@(assign-homes-instr* env instr*))]
    [_
     (raise-mismatch-error 'assign-homes-pass-R1 'block b)]))

(define (assign-homes-instr* env i*)
  (map (lambda (i) (assign-homes-instr env i)) i*))

(define (assign-homes-instr env i)
  (match i
    [`(jmp ,l) i]
    [`(callq ,l) i]
    [`(,x86-operator ,a* ...)
     `(,x86-operator ,@(map (lambda (a) (assign-homes-arg env a)) a*))]
    [_
     (raise-mismatch-error 'assign-homes-pass-R1 'instr i)]))

(define (assign-homes-arg env a)
  (match a
    [`(int ,_) a]
    [`(reg ,_) a]
    [`(var ,v) `(deref rbp ,(dict-ref env v))]
    [_
     (raise-mismatch-error 'assign-homes-pass-R1 'arg a)]))

;; Aux

(define (locals->homes var*)
  (let loop ([env '()]
             [v var*]
             [l 0])
    (cond
      [(empty? v)
       (values env (abs l))]
      [else
       (define r (- l 8))
       (loop (cons (cons (first v) r) env)
             (rest v)
             r)])))
