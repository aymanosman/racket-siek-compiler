#lang racket

(provide uncover-locals-pass-R1)

(define (uncover-locals-pass-R1 p)
  (match p
    [`(program ()
        ((start . ,tail)))
     `(program ((locals . ,(uncover-locals-tail tail)))
        ((start . ,tail)))]
    [_
      ((current-R1-mismatch-handler) 'top p)]))

(define (uncover-locals-tail t)
  (remove-duplicates
   (match t
     [`(return ,e) '()]
     [`(seq (assign ,var ,e) ,tail)
      (cons var (uncover-locals-tail tail))]
     [_
       ((current-R1-mismatch-handler) 'tail t)])))

;; Aux

(define current-R1-mismatch-handler
  (make-parameter
    (lambda (kind term)
      (raise-arguments-error 'uncover-locals-pass-R1
        "failed to match"
        "kind" kind
        "term" term))))