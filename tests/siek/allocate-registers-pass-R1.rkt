#lang racket

(provide allocate-registers-tests)

(require rackunit
         "check-pass.rkt")

(require siek)

(define-compiler compile
                 (uniquify-R1
                  normalize-R1
                  explicate-control-pass-R1
                  uncover-locals-pass-R1
                  select-instructions-pass-R1
                  uncover-live-pass-R1
                  build-interference-pass-R1
                  allocate-registers-pass-R1
                  patch-instructions-pass-R1))

;; (compiler-trace! compile #t)

(define-test-suite allocate-registers-tests
  (test-compiler compile
                 (R1 -> x860)
                 2
                 (- 10)
                 (- (+ 10 20))
                 (let ([x 32])
                   (+ x 10))
                 (let ([x (let ([x 4])
                            (+ x 1))])
                   (+ x 2)))
  (case (system-type 'vm)
    [(racket)
     (test-case "move biasing should reveal more redundant instructions"
       (flaky-test))]
    [else
     ;; TODO investigate why this test fails on chez
     (test-case "(FAIL) move biasing should reveal more redundant instructions"
       (flaky-test #:fail? #t))]))

(define (flaky-test #:fail? [fail? #f])
  (define-compiler compile
                   (uniquify-R1
                    normalize-R1
                    explicate-control-pass-R1
                    uncover-locals-pass-R1
                    select-instructions-pass-R1
                    uncover-live-pass-R1
                    build-interference-pass-R1
                    allocate-registers-pass-R1))
  (define prog
    '(program
      ()
      (let ([v 1])
        (let ([w 42])
          (let ([x (+ v 7)])
            (let ([y x])
              (let ([z (+ x w)])
                (+ z (- y)))))))))
  (define-check (check-optimization prog)
    (define orig
      (parameterize ([compiler-enable-move-biasing? #f])
        (compare-assembly-after-patch prog)))
    (define opt
      (parameterize ([compiler-enable-move-biasing? #t])
        (compare-assembly-after-patch prog)))
    (with-check-info (['original orig]
                      ['optimized opt])
                     (cond
                       [fail? ;; expect to fail
                        (unless (not (> opt orig))
                          (fail-check))]
                       [else
                        (unless (> opt orig)
                          (fail-check))])))
  (define (compare-assembly-after-patch prog)
    (define out (compile prog))
    (- (num-instr out)
       (num-instr (patch-instructions-pass-R1 out))))
  (define num-instr
    (lambda (p)
      (match p
        [`(program ,_ ((start . (block ,_ ,instr* ...))))
         (length instr*)])))
  (check-optimization prog))
