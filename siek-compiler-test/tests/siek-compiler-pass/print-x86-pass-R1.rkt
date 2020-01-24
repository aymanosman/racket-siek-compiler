#lang racket

(require rackunit)

(require siek-compiler-pass/print-x86-pass-R1)

(require siek-compiler-pass/uniquify-pass-R1
         siek-compiler-pass/remove-complex-opera-pass-R1
         siek-compiler-pass/explicate-control-pass-R1
         siek-compiler-pass/uncover-locals-pass-R1
         siek-compiler-pass/select-instructions-pass-R1
         siek-compiler-pass/assign-homes-pass-R1
         siek-compiler-pass/patch-instructions-pass-R1
         siek-compiler-pass/print-x86-pass-R1)

(define compile (compose1 print-x86-pass-R1
                          patch-instructions-pass-R1
                          assign-homes-pass-R1
                          select-instructions-pass-R1
                          uncover-locals-pass-R1
                          explicate-control-pass-R1
                          remove-complex-opera*-pass-R1
                          uniquify-pass-R1))

(define dir (make-temporary-file "siek-compiler-~a" 'directory))

(define-syntax-rule (main e)
  (check-equal?
   (main-fun 'e)
   e))

(define-syntax-rule (main* e ...)
  (begin
    (main e) ...))

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

;; Aux

(define (gcc . args)
  (void
   (apply system* (find-executable-path "gcc") args)))

;; Main

(main*
 2

 (+ 10 20)

 (+ 52 (- 10))

 (let ([x 32])
   (+ x 10))

 (let ([x (let ([x 4])
            (+ x 1))])
   (+ x 2)))
