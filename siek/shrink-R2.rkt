#lang racket

(provide shrink-R2)

(require "options.rkt")

(define (shrink-R2 p)
  (send (new shrink-R2%) shrink p))

(define shrink-R2%
  (class object%
    (super-new)

    (define/public (who)
      'shrink)

    (define/public (shrink p)
      (match p
        [`(program ,info ,e)
         `(program ,info ,(shrink-exp e))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (shrink-exp e)
      (match e
        [`(- ,e0 ,e1)
         `(+ ,e0 (- ,e1))]
        [`(<= ,e0 ,e1)
         (define tmp (fresh 'tmp))
         `(let ([,tmp ,e0])
            (not (< ,e1 ,tmp)))]
        [_ e]))))
