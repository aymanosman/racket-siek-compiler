#lang racket

(provide define-compiler
         compiler-passes
         compiler-trace!)

(require "program.rkt"
         "inspect.rkt"
         "typecheck-R.rkt"
         "shrink.rkt"
         "uniquify.rkt"
         "uniquify.rkt"
         "normalize.rkt"
         "normalize.rkt"
         "explicate-control.rkt"
         "remove-jumps.rkt"
         "uncover-locals.rkt"
         "uncover-live.rkt"
         "uncover-conflicts.rkt"
         "select-instructions.rkt"
         "assign-homes.rkt"
         "assign-colors.rkt"
         "allocate-registers.rkt"
         "patch-instructions.rkt")

(struct compiler (proc passes [trace? #:mutable])
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
     (define port (and trace?
                       (cond
                         [(output-port? trace?)
                          trace?]
                         [else
                          (current-output-port)])))

     (when trace?
       (fprintf port "=== input ===\n")
       (parameterize ([pretty-print-depth #f])
         (pretty-print program port)))

     (for/fold ([prog program])
               ([pass pass*])
       (define p (pass prog))
       (when trace?
         (fprintf port "=== ~a ===\n" (object-name pass))
         ;; TODO check the output of each pass
         ;; Problem: how to handle (read)
         ;; Solution: interp should escape to read hander??) (current-read-handler)
         (parameterize ([pretty-print-depth #f])
           (write-pass port pass p)))
       p))
   pass*
   #f))

(define (compiler-trace! c v)
  (set-compiler-trace?! c v))

(define (write-pass port pass p)
  (cond
    [(member pass (list select-instructions-R1
                        select-instructions-R2
                        assign-homes-R1
                        assign-homes-R2
                        uncover-live-R1
                        uncover-live-R2
                        uncover-conflicts-R1
                        uncover-conflicts-R2
                        assign-colors-R1
                        assign-colors-R2
                        allocate-registers-R1
                        allocate-registers-R2
                        patch-instructions-R1
                        patch-instructions-R2))
     (pretty-print (program-info p) port)
     (write-x86 port p)]
    [(member pass (list explicate-control-R1
                        explicate-control-R2
                        remove-jumps-R1
                        remove-jumps-R2
                        uncover-locals-R1
                        uncover-locals-R2))
     (pretty-print (program-info p) port)
     (write-C port p)]
    [(member pass (list typecheck-R0
                        typecheck-R1
                        typecheck-R2
                        shrink-R2
                        uniquify-R1
                        uniquify-R2
                        normalize-R1
                        normalize-R2))
     (pretty-print (program-info p) port)
     (write-R port p)]
    [else
     (pretty-write p port)]))
