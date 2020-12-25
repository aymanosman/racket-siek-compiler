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

(define-syntax main
  (syntax-rules (<=)
    [(_ [e <= input])
     (test-case (format "~a" 'e)
       (check-equal?
        (main-fun 'e input)
        (parameterize ([current-input-port (open-input-string input)])
          e)))]
    [(_ e)
     (test-case (format "~a" 'e)
       (check-equal?
        (main-fun 'e)
        e))]))

(define-x86-test-suite print-x86-tests
                       2
                       (+ 10 20)
                       (+ 52 (- 10))
                       [(read) <= "78"]
                       [(+ (read) (read)) <= "1 2"]
                       (let ([x 32])
                         (+ x 10))
                       (let ([x (let ([x 4])
                                  (+ x 1))])
                         (+ x 2)))

(define dir (make-temporary-file "siek-compiler-~a" 'directory))

(define (main-fun expr [input #f])
  (parameterize ([current-directory dir])
    (define stst (make-temporary-file "stst-~a" #f (current-directory)))
    (define stst.s (path-add-extension stst #".s"))
    (define stst.o (path-add-extension stst #".o"))
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
