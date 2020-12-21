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

    (define assembly (parameterize ([current-gensym (make-gensym)])
         (compile
          '(program
            ()
            (let ([v 1])
              (let ([w 42])
                (let ([x (+ v 7)])
                  (let ([y x])
                    (let ([z (+ x w)])
                      (+ z (- y)))))))))))

    (define num-instr
      (lambda (p)
        (match p
          [`(program ,_ ((start . (block ,_ ,instr* ...))))
           (length instr*)])))

    (test-case "move biasing should reveal 3 redundant instructions"
      (check-equal?
       (- (num-instr assembly)
          (num-instr (patch-instructions-pass-R1 assembly)))
       3))))
