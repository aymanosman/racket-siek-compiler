#lang racket

(provide inspect-tests)

(require rackunit
         siek/inspect)

(module+ test
  (require rackunit/text-ui)
  (run-tests inspect-tests))

(define-check (check-inspect write-proc self out-str)
  (define actual (let ([port (open-output-string)])
                    (write-proc port self)
                    (get-output-string port)))
  (unless (equal? actual out-str)
    (with-check-info (['self self]
                      ['actual actual]
                      ['expected out-str])
      (fail-check))))

(define-test-suite inspect-tests
  (test-case "write-x86"
    (check-inspect write-x86
                   '(program
                     ()
                     ((start
                       .
                       (block
                        ()
                        (movq (int 1) (reg rax))
                        (jmp conclusion)))))
                   ";; x86
start:
  movq $1, %rax
  jmp conclusion\n"))

  (test-case "write-C"
    (check-inspect write-C
                   '(program
                     ()
                     ((start . (if (< 1 2)
                                   (goto then)
                                   (goto else)))
                      (then . (return 1))
                      (else . (return 2))))
                   ";; C
start:
  if (< 1 2):
     goto then;
     goto else;
then:
  return 1;
else:
  return 2;\n"))

(test-case "write-C (2)"
    (check-inspect write-C
                   '(program
                     ()
                     ((start . (seq (assign x 1) (goto start2)))
                      (start2 . (if (< x 2)
                                    (goto then)
                                    (goto else)))
                      (then . (return 1))
                      (else . (return 2))))
                   ";; C
start:
  x = 1;
  goto start2;
start2:
  if (< x 2):
     goto then;
     goto else;
then:
  return 1;
else:
  return 2;\n"))
  )
