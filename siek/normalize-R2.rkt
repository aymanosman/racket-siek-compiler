#lang racket

(provide normalize-R2%
         normalize-R2)

(require "normalize-R1.rkt"
         "R2.rkt")

(define (normalize-R2 p)
  (send (new normalize-R2%) normalize p))

(define normalize-R2%
  (class normalize-R1%
    (super-new)

    (inherit normalize-op arg-op add-lets)

    (define/override (who)
      'normalize-R2)

    (define/override (normalize-exp e)
      (match e
        [(? boolean?)
         e]
        [`(- ,e0 ,e1)
         (normalize-op '- e0 e1)]
        [`(and ,e0 ,e1)
         (normalize-op 'and e0 e1)]
        [`(or ,e0 ,e1)
         (normalize-op 'or e0 e1)]
        [`(not ,e)
         (normalize-op 'not e)]
        [`(,c ,e0 ,e1)
         #:when (cmp? c)
         (normalize-op c e0 e1)]
        [`(if ,e0 ,e1 ,e2)
         `(if ,(normalize-exp e0)
              ,(normalize-exp e1)
              ,(normalize-exp e2))]
        [_
         (super normalize-exp e)]))

    (define/override (normalize-arg e)
      (match e
        [(? boolean?)
         (values e '())]
        [`(- ,e0 ,e1)
         (arg-op '- e0 e1)]
        [`(and ,e0 ,e1)
         (arg-op 'and e0 e1)]
        [`(or ,e0 ,e1)
         (arg-op 'or e0 e1)]
        [`(not ,e)
         (arg-op 'not e)]
        [`(,c ,e0 ,e1)
         #:when (cmp? c)
         (arg-op c e0 e1)]
        [_
         (super normalize-arg e)]))))
