#lang racket

(provide select-instructions-pass-R1)

(require "raise-mismatch-error.rkt")

(define (select-instructions-pass-R1 p)
  (match p
    [`(program ,info ((start . ,tail)))
     `(program
       ,info
       ((start .
               (block ()
                      ,@(select-instructions-tail tail)
                      (jmp conclusion)))))]
    [_
     (raise-mismatch-error 'select-instructions-pass-R1 'top p)]))

(define (select-instructions-tail t)
  (match t
    [`(return ,e)
     (select-instructions-stmt `(assign (reg rax) ,e))]
    [`(seq ,stmt ,tail)
     (append (select-instructions-stmt stmt)
             (select-instructions-tail tail))]
    [_
     (raise-mismatch-error 'select-instructions-pass-R1 'tail t)]))

(define (select-instructions-stmt stmt)
  (match stmt
    [`(assign ,v ,e)
     (define r
       (match v
         [`(reg rax) v]
         [(? symbol?) `(var ,v)]
         [_
          (raise-mismatch-error 'select-instructions-pass-R1 'var v)]))
     (match e
       ;; Arg
       [(? fixnum?) (list `(movq (int ,e) ,r))]
       [(? symbol?) (list `(movq (var ,e) ,r))]
       ;; read
       [`(read)
        (cons `(callq read_int)
              (cond
                [(equal? '(reg rax) r)
                 '()]
                [else
                 (list `(movq (reg rax) ,r))]))]
       ;; -
       [`(- ,a)
        (list `(movq ,(select-instructions-arg a) ,r)
              `(negq ,r))]
       ;; +
       [`(+ ,(== v) ,a)
        (list `(addq ,(select-instructions-arg a) ,r))]
       [`(+ ,a ,(== v))
        (list `(addq ,(select-instructions-arg a) ,r))]
       [`(+ ,a0 ,a1)
        (list `(movq ,(select-instructions-arg a0) ,r)
              `(addq ,(select-instructions-arg a1) ,r))])]
    [_
     (raise-mismatch-error 'select-instructions-pass-R1 'stmt stmt)]))

(define (select-instructions-arg a)
  (match a
    [(? fixnum?) `(int ,a)]
    [(? symbol?) `(var ,a)]
    [_
     (raise-mismatch-error 'select-instructions-pass-R1 'arg a)]))
