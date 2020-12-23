#lang racket

(provide x861?
         x861*?
         interp-x861
         interp-x861*
         format-x861)

(require "x860.rkt"
         "match-instr.rkt"
         "define-x86.rkt"
         threading
         "options.rkt")

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

    (define/override (who-interp)
      (if (compiler-psuedo-x86?)
          'interp-x861*
          'interp-x861))

    (define/override (interp-instr env i)
      (match i
        [`(xorq ,a0 ,a1)
         (interp-op env 'xorq (l-value a1) (r-value env a1) (r-value env a0))]
        [`(cmpq ,a0 ,a1)
         #:when
         (member (first a1) '(var reg))
         (interp-op env 'cmpq 'eflags (r-value env a1) (r-value env a0))]
        [(set cc a)
         (dict-set env (l-value a) (interp-eflags env cc))]
        ;; [`(setl ,a)
        ;;  (dict-set env (l-value a) (interp-eflags env 'l))]
        [`(movzbq ,a0 ,a1)
         ;; TODO no need to treat specially?
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
