#lang racket

(provide uncover-locals-R1)

(require "raise-mismatch-error.rkt")

(define (uncover-locals-R1 p)
  (match p
    [`(program
       ()
       ((start . ,tail)))
     `(program
       ((locals . ,(uncover-locals-tail tail)))
       ((start . ,tail)))]
    [_
     (raise-mismatch-error 'uncover-locals-R1 'top p)]))

(define (uncover-locals-tail t)
  (remove-duplicates
   (match t
     [`(return ,e) '()]
     [`(seq (assign ,var ,e) ,tail)
      (cons var (uncover-locals-tail tail))]
     [_
      (raise-mismatch-error 'uncover-locals-R1 'tail t)])))
