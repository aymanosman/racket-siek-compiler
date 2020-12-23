#lang racket

(provide arg
         set)

(define-match-expander arg
  (lambda (stx)
    (syntax-case stx ()
      ((_ p)
       #'(or
          `(var ,p)
          `(reg ,p))))))

(define-match-expander set
  (lambda (stx)
    (syntax-case stx ()
      [(_ cc a)
       #'`(,(? set? (app set->cc cc)) ,a)])))

(define (set? op)
  (and (member op setccs)))

(define (set->cc op)
  (case op
    [(sete) 'e]
    [(setl) 'l]
    [(setle) 'le]
    [(setg) 'g]
    [(setge) 'ge]))

(define (cc->set cc)
  (case cc
    [(e) 'sete]
    [(l) 'setl]
    [(le) 'setle]
    [(g) 'setg]
    [(ge) 'setge]))

(define setccs
  (map cc->set '(e l le g ge)))