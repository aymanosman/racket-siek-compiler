#lang racket

(provide arg
         setcc)

(define-match-expander arg
  (lambda (stx)
    (syntax-case stx ()
      ((_ p)
       #'(or
          `(var ,p)
          `(reg ,p)
          `(bytereg ,p))))))

(define-match-expander setcc
  (lambda (stx)
    (syntax-case stx ()
      [(_ cc a)
       #'`(,(? set? (app set->cc cc)) ,a)])))

(define (set? op)
  (and (member op '(sete setl setle setg setge))))

(define (set->cc op)
  (case op
    [(sete) 'e]
    [(setl) 'l]
    [(setle) 'le]
    [(setg) 'g]
    [(setge) 'ge]))
