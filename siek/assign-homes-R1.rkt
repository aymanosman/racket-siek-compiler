#lang racket

(provide assign-homes-R1)

(require "assign-homes.rkt"
         "raise-mismatch-error.rkt")

(define (assign-homes-R1 p)
  (match p
    [`(program ,info ((start . ,block)))
     (define-values (env stack-space) (locals->homes (dict-ref info 'locals)))
     `(program
       ()
       ((start . ,(assign-homes-block env stack-space block))))]
    [_
     (raise-mismatch-error 'assign-homes-R1 'top p)]))

(define (assign-homes-block env stack-space b)
  (match b
    [`(block ,_ ,instr* ...)
     ;; TODO do stack-space properly
     `(block ((stack-space . ,stack-space)) ,@(assign-homes env instr*))]
    [_
     (raise-mismatch-error 'assign-homes-R1 'block b)]))

(define (locals->homes var*)
  (let loop ([env '()]
             [v var*]
             [l 0])
    (cond
      [(empty? v)
       (values env (abs l))]
      [else
       (define r (- l 8))
       (loop (dict-set env (first v) `(deref rbp ,r))
             (rest v)
             r)])))
