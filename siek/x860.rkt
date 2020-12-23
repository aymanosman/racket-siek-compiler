#lang racket

(provide x860%
         x860?
         x860*?
         interp-x860
         interp-x860*
         format-x860)

(require racket/fixnum
         racket/undefined
         threading
         "define-x86.rkt"
         "block.rkt"
         "options.rkt"
         "raise-mismatch-error.rkt")

;; x860
;; instr := (addq a a) | (subq a a) | (negq a) | (movq a a)
;;        | (callq l) | (pushq a) | (popq a) | (retq) | (jmp l)
;; arg   := (int n) | (reg r) | (deref r n)

(define-x86 x860%)

(define x860%
  (class object%
    (super-new)

    (define/public (? p)
      (match p
        [`(program ,_ ((start . ,block)))
         (block? block)]
        [_ #f]))

    (define/public (block? b)
      (match b
        [`(block ,_ ,instr* ...)
         (andmap (lambda (i) (instr? i)) instr*)]
        [_ #f]))

    (define/public (instr? i)
      (match i
        [`(movq ,a0 ,a1) (and (arg? a0) (arg? a1))]
        [`(negq ,a) (arg? a)]
        [`(addq ,a0 ,a1) (and (arg? a0) (arg? a1))]
        [`(subq ,a0 ,a1) (and (arg? a0) (arg? a1))]
        [`(retq) #t]
        [`(callq ,label)
         #:when
         (symbol? label)
         #t]
        [`(pushq ,a) (arg? a)]
        [`(popq ,a) (arg? a)]
        [_ #f]))

    (define/public (arg? a)
      (match a
        [`(int ,n) #:when (fixnum? n) #t]
        [`(reg ,r) #:when (register? r) #t]
        [`(deref ,r ,n) #:when (and (register? r) (fixnum? n)) #t]
        [`(var ,v)
         #:when
         (var? v)
         #t]
        [_ #f]))

    (define/public (var? v)
      (and (compiler-psuedo-x86?)
           (symbol? v)))

    (define/public (register? r)
      (and (member r '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)) #t))

    (define/public (who-interp)
      (if (compiler-psuedo-x86?)
          'interp-x860*
          'interp-x860))

    (define/public (interp p)
      (dict-ref (interp/env p)
                'rax
                (lambda ()
                  (raise-arguments-error (who-interp) "invalid rax"))))

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
         (raise-mismatch-error (who-interp) 'top p)]))

    (define/public (interp-block code env b)
      (match b
        [`(block ,_ ,instr* ...)
         (interp-instr* code env instr*)]
        [_
         (raise-mismatch-error (who-interp) 'block b)]))

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
            (raise-arguments-error (who-interp)
                                   "undefined builtin"
                                   "label"
                                   l)])]
        [_
         (raise-mismatch-error (who-interp) 'instr i)]))

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
         (raise-mismatch-error (who-interp) 'l-value a)]))

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
         (raise-mismatch-error (who-interp) 'r-value a)]))

    (define/public (format p)
      (match p
        [`(program ,_ ,cfg)
         (with-output-to-string
          (thunk
           (newline)
           (format-cfg cfg)))]))

    (define/public (format-cfg cfg)
      ;; TODO topological order?
      (for ([s cfg])
        (match-define (cons label block) s)
        (printf "~a:\n" label)
        (format-block block)))

    (define/public (format-block b)
      (match b
        [`(block ,_ ,instr* ...)
         (for ([i instr*])
           (format-instr i))]))

    (define/public (format-instr i)
      (match i
        [`(callq ,l)
         (printf "    callq ~a" l)]
        [`(jmp ,l)
         (printf "    jmp ~a" l)]
        [`(,op ,a)
         (printf "    ~a " op)
         (format-arg a)
         (printf "\n")]
        [`(,op ,a0 ,a1)
         (printf "    ~a " op)
         (format-arg a0)
         (printf ", ")
         (format-arg a1)
         (printf "\n")]))

    (define/public (format-arg a)
      (match a
        [`(int ,n)
         (printf "$~a" n)]
        [`(reg ,r)
         (printf "%~a" r)]
        ;; TODO move
        [`(bytereg ,r)
         (printf "%~a" r)]
        [`(var ,v)
         (printf "~a" v)]
        [`(deref ,r ,n)
         (printf "~a(%~a)" n r)]))))
