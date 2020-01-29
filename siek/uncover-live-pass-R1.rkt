#lang racket

(provide uncover-live-pass-R1)

(define (uncover-live-pass-R1 p)
  (match p
    [`(program ,info ,code)
     `(program ,info
        ,(map (match-lambda [(cons label block)
                             (cons label (uncover-live-block block))])
                           code))]
    [_ (report-mismatch-error 'top p)]))

(define (uncover-live-block b)
  (match b
    [`(block () ,instr* ...)
     `(block ((live-afters . ,(live-afters instr*)))
        ,@instr*)]
    [_ (report-mismatch-error 'block b)]))

;; Aux

(define (live-afters instr*)
  (live-afters/aux (set) (list (set)) (reverse instr*)))

(define (live-afters/aux pre acc i*)
  (match i*
    ['() acc]
    [(cons i i*)
     (define next (N pre i))
     (live-afters/aux next (cons next acc) i*)]))

(define (N pre i)
  (define-values (W R) (W+R i))
  (set-union (set-subtract pre W) R))

(define (W+R i)
  (match i
    [`(jmp ,_)       (none)]
    [`(,op)          (none)]
    [`(negq ,a)      (read+write a)]
    [`(addq ,a0 ,a1) (combine (read a0) (read+write a1))]
    [`(movq ,a0 ,a1) (combine (read a0) (write a1))]
    [_ (report-mismatch-error 'instr i)]))

(define (report-mismatch-error kind term)
  (raise-arguments-error 'uncover-live-pass-R1 "failed match"
                         "kind" kind
                         "term" term))

(define (none) (values (set) (set)))
(define (read-fun v) (values (set) (set v)))
(define (write-fun v) (values (set v) (set)))
(define (read+write-fun v) (values (set v) (set v)))

(define-syntax-rule (read a)
  (match a
    [`(var ,v) (read-fun v)] [_ (none)]))
(define-syntax-rule (write a)
  (match a
    [`(var ,v) (write-fun v)] [_ (none)]))
(define-syntax-rule (read+write a)
  (match a
    [`(var ,v) (read+write-fun v)] [_ (none)]))

(define-syntax combine
  (syntax-rules ()
    [(_ e0 e1)
     (begin
       (define-values (r0 w0) e0)
       (define-values (r1 w1) e1)
       (values (set-union r0 r1) (set-union w0 w1)))]))
