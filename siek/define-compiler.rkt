#lang racket

(provide define-compiler
         compiler-trace!)

(struct compiler (proc [trace? #:mutable])
  #:property
  prop:procedure
  (lambda (self program #:trace? [trace? #f])
    ((compiler-proc self) program #:trace? (or trace? (compiler-trace? self)))))

(define-syntax-rule (define-compiler id
                      (pass ...))
  (define id
    (define-compiler-proc (list pass ...))))

(define (define-compiler-proc pass*)
  (compiler
   (lambda (program #:trace? trace?)
     (for/fold ([prog program])
               ([pass pass*])
       (define out (pass prog))
       (when trace?
         (printf "=== ~a ===\n" (object-name pass))
         (parameterize ([pretty-print-depth #f])
           (pretty-print out)))
       out))
   #f))

(define (compiler-trace! c v)
  (set-compiler-trace?! c v))
