#lang racket

(provide allocate-registers-tests)

(require rackunit
         "check-pass.rkt")

(require siek
         siek/gensym)

(define-compiler compile
                 (uniquify-pass-R1
                  remove-complex-opera*-pass-R1
                  explicate-control-pass-R1
                  uncover-locals-pass-R1
                  select-instructions-pass-R1
                  uncover-live-pass-R1
                  build-interference-pass-R1
                  allocate-registers-pass-R1
                  patch-instructions-pass-R1))

;; (compiler-trace! compile #t)

(define-test-suite allocate-registers-tests
  ;; TODO rename check-pass* test-compiler
  (check-pass* compile
               (R1 -> x860)
               2
               (- 10)
               (- (+ 10 20))
               (let ([x 32])
                 (+ x 10))
               (let ([x (let ([x 4])
                          (+ x 1))])
                 (+ x 2)))
  (let ()
    (define-compiler compile
                     (uniquify-pass-R1
                      remove-complex-opera*-pass-R1
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
                       (unless (> opt orig)
                         (fail-check))))
    (define (compare-assembly-after-patch prog)
      (define out (compile prog))
      (- (num-instr out)
         (num-instr (patch-instructions-pass-R1 out))))
    (define num-instr
      (lambda (p)
        (match p
          [`(program ,_ ((start . (block ,_ ,instr* ...))))
           (length instr*)])))
    (test-case "move biasing should reveal more redundant instructions"
      (check-optimization prog))))
