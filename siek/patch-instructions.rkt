#lang racket

(provide patch-instructions-R1
         patch-instructions-R2)

(module+ for-test
  (provide patch-instructions-instr))

(require "match-instr.rkt"
         "raise-mismatch-error.rkt")

(define (patch-instructions-R1 p)
  (send (new patch-instructions-R1%) patch p))

(define (patch-instructions-R2 p)
  (send (new patch-instructions-R2%) patch p))

(define (patch-instructions-instr i)
  (send (new patch-instructions-R1%) patch-instr i))

(define patch-instructions-R1%
  (class object%
    (super-new)

    (define/public (who)
      'patch-instructions-R1)

    (define/public (patch p)
      (match p
        [`(program ,info ,code)
         `(program ,info ,(map (match-lambda
                                 [(cons label block)
                                  (cons label (patch-block block))])
                               code))]
        [_ (raise-mismatch-error (who) 'top p)]))

    (define/public (patch-block b)
      (match b
        [`(block ,info ,instr* ...)
         `(block ,info ,@(append-map (lambda (i) (patch-instr i)) instr*))]
        [_ (raise-mismatch-error (who) 'block b)]))

    (define/public (patch-instr i)
      (match i
        [`(,op ,a)
         (list i)]
        [`(,op (deref ,r0 ,a0) (deref ,r1 ,a1))
         `((movq (deref ,r0 ,a0) (reg rax))
           (,op (reg rax) (deref ,r1 ,a1)))]
        [`(movq ,(arg a0) ,(arg a1)) #:when (symbol=? a0 a1)
                                     empty]
        [`(,op ,a0 ,a1)
         (list i)]
        [_
         (raise-mismatch-error (who) 'instr i)]))))

(define patch-instructions-R2%
  (class patch-instructions-R1%
    (super-new)

    (define/override (who)
      'patch-instructions-R2)))
