#lang racket

(provide uncover-locals-R1
         uncover-locals-R2)

(require "raise-mismatch-error.rkt")

(define (uncover-locals-R1 p)
  (send (new uncover-locals-R1%) uncover p))

(define (uncover-locals-R2 p)
  (send (new uncover-locals-R2%) uncover p))

(define uncover-locals-R1%
  (class object%
    (super-new)

    (define/public (who)
      'uncover-locals-R1)

    (define/public (uncover p)
      (match p
        [`(program ,info ,code)
         (define new-info
           (dict-set info 'locals (append-map (match-lambda [(cons _ tail)
                                                             (remove-duplicates
                                                              (uncover-tail tail))])
                                              code)))
         `(program ,new-info ,code)]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (uncover-tail t)
      (match t
        [`(return ,_) '()]
        [`(seq (assign ,var ,_) ,tail)
         (cons var (uncover-tail tail))]
        [_
         (raise-mismatch-error (who) 'tail t)]))))

(define uncover-locals-R2%
  (class uncover-locals-R1%
    (super-new)

    (define/override (who)
      'uncover-locals-R2)

    (define/override (uncover-tail t)
      (match t
        [`(if ,_ ,_ ,_)
         empty]
        [`(goto ,_)
         empty]
        [_
         (super uncover-tail t)]))))
