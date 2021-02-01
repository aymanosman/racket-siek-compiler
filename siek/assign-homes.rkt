#lang racket

(provide assign-homes-R1
         assign-homes-R2)

(require "assign-homes-x86.rkt"
         "raise-mismatch-error.rkt")

(define (assign-homes-R1 p)
  (send (new assign-homes-R1%) assign p))

(define assign-homes-R2 assign-homes-R1)

(define assign-homes-R1%
  (class object%
    (super-new)

    (define/public (who)
      'assign-homes-R1)

    (define/public (assign p)
      (match p
        [`(program ,info ,code)
         (define-values (env stack-space) (locals->homes (dict-ref info 'locals)))
         `(program
           ,info
           ,(map (match-lambda [(cons label block)
                                (cons label (assign-homes-block env stack-space block))])
                 code))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (assign-homes-block env stack-space b)
      (match b
        [`(block ,info ,instr* ...)
         ;; TODO do stack-space properly
         `(block
           ,(dict-set info 'stack-space stack-space)
           ,@(assign-homes-x86 env instr*))]
        [_
         (raise-mismatch-error (who) 'block b)]))

    (define/public (locals->homes var*)
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
                 r)])))))
