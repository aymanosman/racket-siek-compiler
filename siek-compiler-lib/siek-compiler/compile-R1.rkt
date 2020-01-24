#lang racket

(provide compile-R1)

(require siek-compiler-pass/uniquify-pass-R1
         siek-compiler-pass/remove-complex-opera-pass-R1
         siek-compiler-pass/explicate-control-pass-R1
         siek-compiler-pass/uncover-locals-pass-R1
         siek-compiler-pass/select-instructions-pass-R1
         siek-compiler-pass/assign-homes-pass-R1
         siek-compiler-pass/patch-instructions-pass-R1
         siek-compiler-pass/print-x86-pass-R1)

(define R1->x860/chapter2
  (compose1 patch-instructions-pass-R1
            assign-homes-pass-R1
            select-instructions-pass-R1
            uncover-locals-pass-R1
            explicate-control-pass-R1
            remove-complex-opera*-pass-R1
            uniquify-pass-R1))

(define compile-R1
  (compose1 print-x86-pass-R1
            R1->x860/chapter2))

(module+ main
  (require siek-compiler-options)

  (define expr (read))

  (parameterize ([current-system-type (system-type 'os)])
    (compile-R1 `(program () ,expr))))
