#lang racket

(provide prim-cata
         if-cata
         let-cata)

(require (for-template racket/base))

(define (prim-cata proc-stx stx)
  (syntax-case stx (->)
    [(_ op (x ...) ...)
     #`(list op (app #,proc-stx x ...) ...)]
    [(_ op x ...)
     #`(list op (app #,proc-stx x) ...)]))

(define (if-cata proc-stx stx)
  (syntax-case stx (->)
    [(_ (x ...) ...)
     #`(list #;'if (app #,proc-stx x ...) ...)]
    [(_ x ...)
     #`(list #;'if (app #,proc-stx x) ...)]))

(define (let-cata head-stx var-proc-stx init-proc-stx body-proc-stx stx)
  (syntax-case stx ()
    [(_ ([(x ...) (e ...)]) (body ...))
     #`(list #,head-stx
             (list (list (app #,var-proc-stx x ...)
                         (list (app #,init-proc-stx e ...))))
             (app (lambda (b) (#,body-proc-stx e ... b)) body ...))]
    [(_ ([x e]) body)
     (let-cata head-stx
               var-proc-stx
               init-proc-stx
               body-proc-stx
               #'(_ ([(x) (e)]) (body)))]))
