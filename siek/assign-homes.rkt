#lang racket

(provide assign-homes)

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
