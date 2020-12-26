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
       (check-equal?
        (main-fun compile 'e input)
        (cond
          [input
           (parameterize ([current-input-port (open-input-string input)])
             e)]
          [else e])))]
    [(_ compile e)
     (main compile [e <= #f])]))

(define dir (make-temporary-file "siek-compiler-~a" 'directory))

(define (main-fun compile expr [input #f])
  (parameterize ([current-directory dir])
    (define stst (make-temporary-file "stst-~a" #f (current-directory)))
    (define stst.s (path-add-extension stst #".s"))
    (define stst.in (path-add-extension stst #".in"))
    (with-output-to-file stst.s
      (thunk
       (compile `(program () ,expr))))
    (gcc "-g" "-o" stst stst.s runtime.c)
    (when input
      (with-output-to-file stst.in
        (thunk
         (display input))))

    (if input
        (run-with-input stst.in stst)
        (system*/exit-code stst))))

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
        (with-check-info (['stderr err-string])
          (fail-check))])]
    [else
     (error 'run-with-input "should never get here")]))

(define (gcc . args)
  (void
   (unless (apply system* (find-executable-path "gcc") args)
     (error "gcc failed"))))
