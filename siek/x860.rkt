#lang racket

(provide x860?
         x860*?
         interp-x860
         interp-x860*)

(require racket/fixnum
         racket/undefined
         threading
         "block.rkt"
         "options.rkt"
         "raise-mismatch-error.rkt")

;; x860
;; instr := (addq a a) | (subq a a) | (negq a) | (movq a a)
;;        | (callq l) | (pushq a) | (popq a) | (retq) | (jmp l)
;; arg   := (int n) | (reg r) | (deref r n)

(define (x860? p)
  (send (new x860%) ? p))

(define (interp-x860 p)
  (send (new x860%) interp p))

(define (x860*? p)
  (parameterize ([current-x86 'x860*])
    (x860? p)))

(define (interp-x860* p)
  (parameterize ([current-x86 'x860*])
    (interp-x860 p)))

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
        [`(callq ,label) #:when (symbol? label)
                         #t]
        [`(pushq ,a) (arg? a)]
        [`(popq ,a) (arg? a)]
        [_ #f]))

    (define/public (arg? a)
      (match a
        [`(int ,n) #:when (fixnum? n) #t]
        [`(reg ,r) #:when (register? r) #t]
        [`(deref ,r ,n) #:when (and (register? r) (fixnum? n)) #t]
        [`(var ,v) #:when  (var? v)
                   #t]
        [_ #f]))

    (define/public (var? v)
      (and (equal? (current-x86) 'x860*)
           (symbol? v)))

    (define/public (register? r)
      (and (member r '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)) #t))

    (define/public (who-interp)
      (if (equal? 'x860* (current-x86))
          'interp-x860*
          'interp-x860))

    (define/public (interp p)
      (dict-ref (interp-x860/env p) 'rax (lambda () (raise-arguments-error (who-interp) "invalid rax"))))

    (define/public (interp-x860/env p)
      (match p
        [`(program ,_ ,cfg)
         (define stack-space
           (case (current-x86)
             [(x860*) 0]
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
         (define rsp (- (dict-ref env 'rsp) 8))
         (~> (dict-set env rsp (r-value env a))
             (dict-set _ 'rsp rsp))]
        [`(popq ,a)
         (define rsp (dict-ref env 'rsp))
         (~> (dict-set env (l-value a) (dict-ref env rsp))
             (dict-set _ rsp undefined)
             (dict-set _ 'rsp (+ rsp 8)))]
        [`(callq ,l) #:when (builtin? l)
                     (case l
                       [(read_int)
                        (dict-set env 'rax (read))]
                       [else
                        (raise-arguments-error (who-interp) "undefined builtin"
                                               "label" l)])]
        [_
         (raise-mismatch-error (who-interp) 'instr i)]))

    (define/public (builtin? l)
      (and (member l '(read_int)) #t))

    (define (l-value a)
      (match a
        [`(reg ,r) r]
        [`(deref ,_ ,m) m]
        [`(var ,v)
         #:when
         (equal? 'x860* (current-x86))
         v]
        [_
         (raise-mismatch-error (who-interp) 'l-value a)]))

    (define/public (r-value env a)
      (match a
        [`(int ,n) #:when (fixnum? n) n]
        [`(reg ,r) #:when (register? r) (dict-ref env r)]
        [`(deref ,r ,m) #:when (register? r) (dict-ref env m)] ;; TODO not using r
        [`(var ,v) #:when (var? v)
         (dict-ref env v)]
        [_
         (raise-mismatch-error (who-interp) 'r-value a)]))))
