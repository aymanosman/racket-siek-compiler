#lang typed/racket/no-check

(provide C
         write-tail
         (rename-out [C C0]))

(: write-C (-> C Output-Port Any Void))
(define (write-C self port mode)
  (match self
    [(C `(program ,_ ,code))
     (fprintf port ";; C\n")
     (for ([label+block (in-list code)])
       (match-define (cons label block) label+block)
       (fprintf port "~a:\n" label)
       (write-tail port block))]))

(struct C ([program : Any])
  #:property
  prop:custom-write
  write-C)

(: write-tail (-> Output-Port Any Void))
(define (write-tail port tail)
  (match tail
    [`()
     (void)]
    [`(seq (assign ,x ,v) ,tail)
     (fprintf port "  ~a = ~a;\n" x v)
     (write-tail port tail)]
    [`(return ,a)
     (fprintf port "  return ~a;\n" a)]
    [`(goto ,l)
     (fprintf port "  goto ~a;\n" l)]
    [`(if ,e (goto ,l0) (goto ,l1))
     (fprintf port "  if ")
     (write-exp port e)
     (fprintf port ":\n")
     (fprintf port "     goto ~a;\n" l0)
     (fprintf port "     goto ~a;\n" l1)]))

(: write-exp (-> Output-Port Any Void))
(define (write-exp port e)
  (match e
    [(? fixnum?) (fprintf port "~a" e)]
    [(? boolean?) (fprintf port "~a" e)]
    [`(< ,a0 ,a1) (fprintf port "(< ~a ~a)" a0 a1)]))