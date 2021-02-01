#lang racket

(provide caller-saved-registers
         callee-saved-registers
         register?

         x860%
         x860?
         x860*?
         interp-x860
         interp-x860*

         x861?
         x861*?
         interp-x861
         interp-x861*
         format-x861)

(require racket/fixnum
         racket/undefined
         threading
         "block.rkt"
         "define-x86.rkt"
         "options.rkt"
         "raise-mismatch-error.rkt")

(define (register? r)
  (set-member? (set-union (caller-saved-registers)
                          (callee-saved-registers))
               r))

(define callee-saved-registers
  (make-parameter
   (set 'rsp 'rbp 'rbx 'r12 'r13 'r14 'r15)))

(define caller-saved-registers
  (make-parameter
   (set 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11)))

;; x860
#;
(define-language x860
  (grammar
   (instr := (addq a a) (subq a a) (negq a) (movq a a)
             (callq l) (pushq a) (popq a) (retq) (jmp l))
   (arg a := (int n) (reg r) (deref r n))))

(define-x86 x860%)

(define x860%
  (class object%
    (super-new)

    (define/public (who)
      (if (compiler-psuedo-x86?)
          'interp-x860*
          'interp-x860))

    (define/public (interp p)
      (dict-ref (interp/env p)
                'rax
                (lambda ()
                  (raise-arguments-error (who) "invalid rax"))))

    (define/public (interp/env p)
      (match p
        [`(program ,_ ,cfg)
         (define stack-space
           (case (compiler-psuedo-x86?)
             [(#t) 0]
             [else
              ;; TODO
              (for/sum ([b (dict-values cfg)])
                (dict-ref (block-info b) 'stack-space))]))
         (define dyld_stub_binder
           '(dyld_stub_binder . (block ())))
         (define conclusion
           `(conclusion
             .
             (block ()
                    (addq (int ,stack-space) (reg rsp))
                    (popq (reg rbp))
                    (retq))))
         (define main
           `(main
             .
             (block ()
                    (pushq (reg rbp))
                    (movq (reg rsp) (reg rbp))
                    (subq (int ,stack-space) (reg rsp))
                    (jmp start))))
         (define code
           (list* conclusion main dyld_stub_binder cfg))
         (interp-block code
                       '((rsp . 0)
                         ;; FIXME (rsp) this would be -8, with 0 containing the return address dyld_stub_binder
                         (rbp . 0)
                         (0 . dyld_stub_binder))
                       (dict-ref code 'main))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (interp-block code env b)
      (match b
        [`(block ,_ ,instr* ...)
         (interp-instr* code env instr*)]
        [_
         (raise-mismatch-error (who) 'block b)]))

    (define/public (interp-instr* code env i*)
      (match i*
        ['()
         env]
        [(cons i i*)
         (match i
           [`(jmp ,l)
            (interp-block code env (dict-ref code l))]
           ['(retq)
            ;; FIXME (rsp)
            (define rsp (dict-ref env 'rsp))
            ;; TODO This should be a representation of a 'return address',
            ;; say (return-address label index)
            (define l (dict-ref env rsp))
            (interp-block code
                          (dict-set env rsp (+ rsp 8))
                          (dict-ref code l))]
           [_
            (interp-instr* code (interp-instr env i) i*)])]))

    (define/public (interp-instr env i)
      (local-require threading)
      (match i
        [`(addq ,a0 ,a1)
         (dict-set env (l-value a1) (fx+ (r-value env a1) (r-value env a0)))]
        [`(subq ,a0 ,a1)
         (dict-set env (l-value a1) (fx- (r-value env a1) (r-value env a0)))]
        [`(movq ,a ,d)
         (dict-set env (l-value d) (r-value env a))]
        [`(negq ,a)
         (dict-set env (l-value a) (fx- 0 (r-value env a)))]
        [`(pushq ,a)
         ;; (rsp) = a
         ;; rsp -= 8
         ;; FIXME (rsp) won't need to do the -8 after fixing?
         (define rsp (- (dict-ref env 'rsp) 8))
         (~> (dict-set env rsp (r-value env a))
             (dict-set _ 'rsp rsp))]
        [`(popq ,a)
         (define rsp (dict-ref env 'rsp))
         (~> (dict-set env (l-value a) (dict-ref env rsp))
             (dict-set _ rsp undefined)
             (dict-set _ 'rsp (+ rsp 8)))]
        [`(callq ,l)
         #:when
         (builtin? l)
         (case l
           [(read_int)
            (dict-set env 'rax (read))]
           [else
            (raise-arguments-error (who)
                                   "undefined builtin"
                                   "label"
                                   l)])]
        [_
         (raise-mismatch-error (who) 'instr i)]))

    (define/public (builtin? l)
      (and (member l '(read_int)) #t))

    (define/public (l-value a)
      (match a
        [`(reg ,r) r]
        [`(deref ,_ ,m) m]
        [`(var ,v)
         #:when
         (compiler-psuedo-x86?)
         v]
        [_
         (raise-mismatch-error (who) 'l-value a)]))

    (define/public (r-value env a)
      (match a
        [`(int ,n) #:when (fixnum? n) n]
        [`(reg ,r) #:when (register? r) (dict-ref env r)]
        [`(deref ,r ,m) #:when (register? r) (dict-ref env m)]
        ;; TODO not using r
        [`(var ,v)
         #:when
         (var? v)
         (dict-ref env v)]
        [_
         (raise-mismatch-error (who) 'r-value a)]))))

(define (var? v)
  (and (compiler-psuedo-x86?)
       (symbol? v)))

;; x861
;; instr   := ...
;;          | (xorq a a) | (cmpq a a) | ( set<cc> a) | (movzbq a a) | (j<cc> l)
;; arg     := ... | (bytereg b)
;; cc      := e | l | le | g | ge
;; bytereg := ah | al | bh | bl | ch | cl | dh | dl

(define-x86 x861%)

(define x861%
  (class x860%
    (super-new)

    (inherit interp-block)

    (define/override (who)
      (if (compiler-psuedo-x86?)
          'interp-x861*
          'interp-x861))

    (define/override (interp-instr* code env i*)
      (match i*
        ['()
         env]
        [(cons i i*)
         (match i
           [`(jl ,l)
            (match (interp-eflags env 'l)
              [1 (interp-block code env (dict-ref code l))]
              [0 (interp-instr* code env i*)])]
           [`(je ,l)
            (match (interp-eflags env 'e)
              [1 (interp-block code env (dict-ref code l))]
              [0 (interp-instr* code env i*)])]
           [_
            (super interp-instr* code env (cons i i*))])]))

    (define/override (interp-instr env i)
      (local-require (only-in "match-instr.rkt" set))
      (match i
        [`(xorq ,a0 ,a1)
         (interp-op env 'xorq (l-value a1) (r-value env a1) (r-value env a0))]
        [`(cmpq ,a0 ,a1)
         #:when (member (first a1) '(var reg deref bytereg))
         (interp-op env 'cmpq 'eflags (r-value env a1) (r-value env a0))]
        [(set cc a)
         (dict-set env (l-value a) (interp-eflags env cc))]
        [`(movzbq ,a0 ,a1)
         (interp-instr env `(movq ,a0 ,a1))]
        [_ (super interp-instr env i)]))

    (define/public (interp-eflags env mode)
      (~> env
          (dict-ref _ 'eflags)
          (symbol=? _ mode)
          boolean->integer))

    (define/override (l-value a)
      (match a
        [`(bytereg ,r)
         #:when
         (byte-register? r)
         r]
        [_ (super l-value a)]))

    (define/override (r-value env a)
      (match a
        [`(bytereg ,r)
         #:when
         (byte-register? r)
         (dict-ref env r)]
        [_ (super r-value env a)]))

    (define/public (byte-register? r)
      (and (member r '(ah al bh bl ch cl dh dl))))

    (define/public (interp-op env op dest a1 a0)
      (define proc
        (case op
          [(xorq) handle-xorq]
          [(cmpq) handle-cmpq]))
      (dict-set env dest (proc a1 a0)))))

(define (handle-xorq a1 a0) (bitwise-xor a1 a0))

(define (handle-cmpq x y)
  (cond
    [(= x y) 'e]
    [(< x y) 'l]
    [(<= x y) 'le]
    [(> x y) 'g]
    [(>= x y) 'ge]))

(define (boolean->integer b)
  (match b
    [#t 1]
    [#f 0]))
