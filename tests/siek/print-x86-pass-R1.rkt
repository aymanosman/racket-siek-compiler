#lang racket

(provide print-x86-tests)

(require rackunit)

(require siek)

(define compile
  (compose1 print-x86-pass-R1
            patch-instructions-pass-R1
            assign-homes-pass-R1
            select-instructions-pass-R1
            uncover-locals-pass-R1
            explicate-control-pass-R1
            remove-complex-opera*-pass-R1
            uniquify-pass-R1))

(define-syntax-rule (define-x86-test-suite id e ...)
  (begin
    (define-test-suite id
      (main e)
      ...)))

(define-syntax-rule (main e)
  (test-case (format "~a" 'e)
    (check-equal?
     (main-fun 'e)
     e)))

(define-x86-test-suite print-x86-tests
                       2
                       (+ 10 20)
                       (+ 52 (- 10))
                       (let ([x 32])
                         (+ x 10))
                       (let ([x (let ([x 4])
                                  (+ x 1))])
                         (+ x 2)))

(define (main-fun expr)
  (parameterize ([current-directory dir])
    (define stst (make-temporary-file "stst-~a" #f (current-directory)))
    (define stst.s (path-add-extension stst #".s"))
    (define stst.o (path-add-extension stst #".o"))
    (with-output-to-file stst.s
      (thunk
       (compile `(program () ,expr))))
    (gcc "-g" "-o" stst stst.s)
    (system*/exit-code stst)))

(define dir (make-temporary-file "siek-compiler-~a" 'directory))

(define (gcc . args)
  (void
   (unless (apply system* (find-executable-path "gcc") args)
     (error "gcc failed"))))
