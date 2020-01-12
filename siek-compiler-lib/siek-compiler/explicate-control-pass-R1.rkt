#lang racket

(provide explicate-control-pass-R1)

(define (explicate-control-pass-R1 p)
  (match p
    [`(program () ,e)
     `(program ()
        ((start . ,(explicate-control-tail e))))]
    [_
     ((current-R1-mismatch-handler) 'top p)]))

(define (explicate-control-tail e)
  (match e
    [(? fixnum?) `(return ,e)]
    [`(read) `(return ,e)]
    [`(- ,e0) `(return ,e)]
    [`(+ ,e0 ,e1) `(return ,e)]
    [(? symbol?) `(return ,e)]
    [`(let ([,x ,e0]) ,e1)
     (explicate-control-assign e0 x (explicate-control-tail e1))]
    [_
     ((current-R1-mismatch-handler) 'tail e)]))

(define (explicate-control-assign e v t)
  (match e
    [`(let ([,x ,e0]) ,e1)
     (explicate-control-assign
      e0 x (explicate-control-assign e1 v t))]
    [(or (? fixnum?)
         (? symbol?)
         `(read)
         `(- ,_)
         `(+ ,_ ,_))
     `(seq (assign ,v ,e) ,t)]
    [_
     ((current-R1-mismatch-handler) 'assign e)]))

(define current-R1-mismatch-handler
  (make-parameter
   (lambda (kind term)
     (raise-arguments-error 'explicate-control-pass-R1 "failed match"
                            "kind" kind
                            "term" term))))
