#lang racket

(require "main.rkt"
         "options.rkt")

(define (compile compiler in)
  (define c
    (case compiler
      [("R1/chapter2")
       compile/R1/chapter2]
      [("R1/chapter3")
       compile/R1/chapter3]
      [else
       (raise-arguments-error 'siek/compile "shouldn't get here")]))
  (parameterize ([current-system-type (system-type 'os)])
    (c `(program () ,(read in)))))

(define compile/R1/chapter2
  (compose1 print-x86-pass-R1
            patch-instructions-pass-R1
            assign-homes-pass-R1
            select-instructions-pass-R1
            uncover-locals-pass-R1
            explicate-control-pass-R1
            remove-complex-opera*-pass-R1
            uniquify-pass-R1))

(define compile/R1/chapter3
  (compose1 print-x86-pass-R1
            patch-instructions-pass-R1
            ;; allocate-registers-pass-R1
            build-interference-pass-R1
            uncover-live-pass-R1
            select-instructions-pass-R1
            uncover-locals-pass-R1
            explicate-control-pass-R1
            remove-complex-opera*-pass-R1
            uniquify-pass-R1))

(module+ main
  (require racket/cmdline)

  (define --compiler (make-parameter #f))

  (define valid-compilers
    (list "R1/chapter2"
          "R1/chapter3"))

  (command-line
   #:program
   "siek/compile"
   #:once-any
   [("--compiler") c "select which compiler to use (one of R1/chapter2 R1/chapter3)" (--compiler c)]
   #:args
   ()
   (unless (member (--compiler) valid-compilers)
     (printf "Must select a valid compiler\n")
     (exit 1))
   (compile (--compiler) (current-input-port))))
