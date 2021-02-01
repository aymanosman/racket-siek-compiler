#lang racket

(provide select-instructions-R1
         select-instructions-R2)

(require "options.rkt"
         "raise-mismatch-error.rkt")

(define (select-instructions-R1 p)
  (send (new select-instructions-R1%) select p))

(define (select-instructions-R2 p)
  (send (new select-instructions-R2%) select p))

(define select-instructions-R1%
  (class object%
    (super-new)

    (define/public (who)
      'select-instructions-R1)

    (define/public (select p)
      (match p
        [`(program ,info ,code)
         `(program
           ,info
           ,(map tail->block code))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define tail->block
      (match-lambda
        [(cons label tail)
         (cons label `(block () ,@(select-tail tail)))]))

    (define/public (select-tail t)
      (match t
        [`(return ,e)
         (append (select-stmt `(assign (reg rax) ,e))
                 (list '(jmp conclusion)))]
        [`(seq ,stmt ,tail)
         (append (select-stmt stmt)
                 (select-tail tail))]
        [_
         (raise-mismatch-error (who) 'tail t)]))

    (define/public (select-stmt stmt)
      (match stmt
        [`(assign ,v ,e)
         (select-assign e v)]
        [_
         (raise-mismatch-error (who) 'stmt stmt)]))

    (define-match-expander prim
      (lambda (stx)
        (syntax-case stx ()
          [(_ op p)
           #'(list (app (op->instr 1) op) p)]
          [(_ op p0 p1)
           #'(list (app (op->instr 2) op) p0 p1)])))

    (define ((op->instr arity) op)
      (case arity
        [(1)
         (case op
           [(-) 'negq]
           [else `(unknown ,op)])]
        [(2)
         (case op
           [(+) 'addq]
           [(-) 'subq]
           [else `(unknown ,op)])]))

    (define/public (select-assign e v)
      (define r (var->destination v))
      (match e
        [(? arg?)
         (list `(movq ,(select-arg e) ,r))]
        [`(read)
         (cons `(callq read_int)
               (cond
                 [(equal? '(reg rax) r)
                  empty]
                 [else
                  (list `(movq (reg rax) ,r))]))]
        [(prim op a)
         (list `(movq ,(select-arg a) ,r)
               `(,op ,r))]
        [(or (prim op (== v) a)
             (prim op a (== v)))
         (list `(,op ,(select-arg a) ,r))]
        [(prim op a0 a1)
         (list `(movq ,(select-arg a0) ,r)
               `(,op ,(select-arg a1) ,r))]))

    (define/public (arg? a)
      (match a
        [(? fixnum?) #t]
        [(? symbol?) #t]
        [`(reg ,_) #t]
        [_ #f]))

    (define/public (select-arg a)
      (match a
        [(? fixnum?) `(int ,a)]
        [(? symbol?) `(var ,a)]
        [`(reg ,_) a]
        [_
         (raise-mismatch-error (who) 'arg a)]))

    (define/public (var->destination v)
      (match v
        [`(reg ,_) v]
        [(? symbol?) `(var ,v)]
        [_
         (raise-mismatch-error (who) 'var v)]))))

(define select-instructions-R2%
  (class select-instructions-R1%
    (super-new)

    (inherit var->destination)

    (define/override (who)
      'select-instructions-R2)

    (define/override (select-tail t)
      (match t
        [`(goto ,l)
         (list `(jmp ,l))]
        [`(if ,(? symbol? a) (goto ,l0) (goto ,l1))
         (append (select-tail-cmp 1 a)
                 (list `(je ,l0) `(jmp ,l1)))]
        [`(if (,c ,a0 ,a1) (goto ,l0) (goto ,l1))
         (append (select-tail-cmp a1 a0)
                 (list (list (cmp->jmp c) l0) `(jmp ,l1)))]
        [_
         (super select-tail t)]))

    (define (cmp->jmp c)
      (case c
        [(<) 'jl]
        [(eq?) 'je]))

    (define/override (select-assign e v)
      (define r (var->destination v))
      (match e
        [`(not ,a)
         (list `(movq ,(select-arg a) ,r)
               `(xorq (int 1) ,r))]
        ;; [`(eq? ,_ ,_)] ;; TODO
        [`(< ,a0 ,a1)
         (list `(movq ,(select-arg a0) ,r)
               `(cmpq ,(select-arg a1) ,r)
               `(setl (bytereg al))
               `(movzbq (bytereg al) ,r))]
        [_
         (super select-assign e v)]))

    (define/override (select-arg a)
      (match a
        [#t '(int 1)]
        [#f '(int 0)]
        [_ (super select-arg a)]))

    (define/public (select-tail-not a)
      (match a
        [(? symbol?)
         (define d (var->destination a))
         (list `(xorq (int 1) ,d)
               `(cmpq (int 0) ,d))]
        [_
         (define t (fresh 'tmp))
         (append (list `(movq ,(select-arg a) (var ,t)))
                 (select-tail-not t))]))

    (define/public (select-tail-cmp a d)
      (match d
        [(? fixnum?)
         (list `(movq ,(select-arg d) (reg rax))
               `(cmpq ,(select-arg a) (reg rax)))]
        [_
         (list `(cmpq ,(select-arg a) ,(select-arg d)))]))))
