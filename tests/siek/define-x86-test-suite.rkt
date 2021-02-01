#lang racket

(provide define-x86-test-suite)

(require rackunit
         siek)

(define-syntax-rule (define-x86-test-suite id compile e ...)
  (begin
    (define-test-suite id
      (main compile e)
      ...)))

(define-syntax main
  (syntax-rules (<=)
    [(_ compile [e <= input])
     (test-case (format "~a" 'e)
       (check-main compile
                   'e
                   (cond
                     [input
                      (parameterize ([current-input-port (open-input-string input)])
                        e)]
                     [else e])
                   input))]
    [(_ compile e)
     (main compile [e <= #f])]))

(define dir (make-temporary-file "siek-compiler-~a" 'directory))

(define-check (check-main compile expr expected input)
  (parameterize ([current-directory dir])
    (define stst (make-temporary-file "stst-~a" #f (current-directory)))
    (define stst.s (path-add-extension stst #".s"))
    (define stst.in (path-add-extension stst #".in"))
    (define trace-out (open-output-string))
    (define asm
      (parameterize ([current-output-port trace-out])
        (compile `(program () ,expr))))
    (with-output-to-file stst.s
      (thunk
       (print-x86 asm)))
    (gcc "-g" "-o" stst stst.s runtime.c)
    (when input
      (with-output-to-file stst.in
        (thunk
         (display input))))

    (if input
        (check-run (run-with-input stst.in stst) expected trace-out)
        (check-run (system*/exit-code stst) expected trace-out))))

(define (check-run actual expected trace-out)
  (unless (equal? actual expected)
    (with-check-info (['actual actual]
                      ['expected expected]
                      ['trace (unquoted-printing-string (get-output-string trace-out))])
      (fail-check))))

(define (run-with-input stst.in stst)
  (match-define (list in out pid err control) (process (format "cat ~a | ~a" stst.in stst)))
  (control 'wait)
  (cond
    [(symbol=? 'done-error (control 'status))
     (define err-string (port->string err))
     (close-input-port in)
     (close-output-port out)
     (close-input-port err)
     (cond
       [(string=? "" err-string)
        (control 'exit-code)]
       [else
        (unquoted-printing-string err-string)])]
    [else
     (error 'run-with-input "should never get here")]))

(define (gcc . args)
  (void
   (unless (apply system* (find-executable-path "gcc") args)
     (error "gcc failed"))))
