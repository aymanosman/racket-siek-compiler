#lang racket

(provide interp-x860
         interp-x860*)

(require racket/undefined
         racket/fixnum)

(require "block.rkt"
         "raise-mismatch-error.rkt")

(define current-x86 (make-parameter 'x860))

(define (interp-x860* p)
  (parameterize ([current-x86 'x860*])
    (interp-x860 p)))

(define (interp-x860 p)
  (dict-ref (interp-x860/env p) 'rax))

(define (interp-x860/env p)
  (match p
    [`(program ,info ,code0)
     (define stack-space
       (case (current-x86)
         [(x860*) 0]
         [else
          ;; TODO
          (for/sum ([b (dict-values code0)])
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
       (list* conclusion main dyld_stub_binder code0))
     (interp-block code
                   '((rsp . 0)
                     (rbp . 0)
                     (0 . dyld_stub_binder))
                   (dict-ref code 'main))]
    [_
     (raise-mismatch-error 'interp-x860 'top p)]))

(define (interp-block code env b)
  (match b
    [`(block ,info ,instr* ...)
     (interp-instr* code env instr*)]
    [_
     (raise-mismatch-error 'interp-x860 'block b)]))

(define (interp-instr* code env i*)
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

(define (interp-instr env i)
  (match i
    [`(addq ,a0 ,a1)
     (dict-set env
               (l-value a1)
               (fx+ (r-value env a1) (r-value env a0)))]
    [`(subq ,a0 ,a1)
     (dict-set env
               (l-value a1)
               (fx- (r-value env a1) (r-value env a0)))]
    [`(movq ,a0 ,a1)
     (dict-set env
               (l-value a1)
               (r-value env a0))]
    [`(negq ,a)
     (dict-set env (l-value a) (fx- 0 (r-value env a)))]
    ;; callq
    [`(pushq ,a)
     (define rsp (- (dict-ref env 'rsp) 8))
     (dict-set
      (dict-set env rsp (r-value env a))
      'rsp
      rsp)]
    [`(popq ,a)
     (define rsp (dict-ref env 'rsp))
     (define val (dict-ref env rsp))
     (dict-set
      (dict-set
       (dict-set env
                 (l-value a)
                 val)
       rsp
       undefined)
      'rsp
      (+ rsp 8))]
    [_
     (raise-mismatch-error 'interp-x860 'instr i)]))

;; Aux

(define (l-value a)
  (match a
    [`(reg ,r) r]
    [`(deref ,reg ,m) m]
    [`(var ,v)
     #:when
     (equal? 'x860* (current-x86))
     v]
    [_
     (raise-mismatch-error 'interp-x860 'l-value a)]))

(define (r-value env a)
  (match a
    [`(int ,n) n]
    [`(reg ,r) (dict-ref env r)]
    [`(var ,v)
     #:when
     (equal? 'x860* (current-x86))
     (dict-ref env v)]
    [`(deref ,reg ,m) (dict-ref env m)]
    [_
     (raise-mismatch-error 'interp-x860 'r-value a)]))
