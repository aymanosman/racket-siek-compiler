#lang typed/racket/no-check

(provide write-x86
         write-C
         write-R)

;;; C

(: write-C (-> Any Output-Port Any Void))
(define (write-C port self)
  (match self
    [`(program ,_ ,code)
     (fprintf port ";; C\n")
     (for ([label+block (in-list code)])
       (match-define (cons label block) label+block)
       (fprintf port "~a:\n" label)
       (write-tail port block))]))

(: write-tail (-> Output-Port Any Void))
(define (write-tail port tail)
  (match tail
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
    [(? atom?) (fprintf port "~a" e)]
    [(and (? prim?) `(,op ,a))
     (fprintf port "(~a ~a)" op a)]
    [(and (? prim?) `(,op ,a0 ,a1))
     (fprintf port "(~a ~a ~a)" op a0 a1)]))

(: atom? (-> Any Boolean))
(define (atom? v)
  (cond
    [(fixnum? v)]
    [(boolean? v)]
    [(symbol? v)]
    [else #f]))

(: prim? (-> Any Boolean))
(define (prim? v)
  (and (list? v)
       (member (first v) '(eq? < <= > >= - + and or not))
       (andmap atom? (rest v))))

;;; x86

(: write-x86 (-> Any Output-Port Any Void))
(define (write-x86 port self)
  (match self
    [`(program ,_ ,code)
     (fprintf port ";; x86\n")
     (for ([label+block (in-list code)])
       (match-define (cons label `(block ,info ,instr* ...)) label+block)
       (fprintf port "~a:\n" label)
       (for ([i instr*])
         (write-instr port i)))]))

(require (for-syntax racket/base))

(define-match-expander instr
  (lambda (stx)
    (syntax-case stx ()
      [(_ op a* ...)
       #'(list op a* ...)])))

(: write-instr (-> Output-Port Any Void))
(define (write-instr port i)
  (match i
    [(instr (and op (or 'callq 'jmp 'je 'jl)) l)
     (fprintf port "  ~a ~a\n" op l)]
    [(instr (and op (or 'movq 'movzbq 'addq 'xorq 'cmpq)) a0 a1)
     (fprintf port "  ~a " op)
     (write-arg port a0)
     (fprintf port ", ")
     (write-arg port a1)
     (newline port)]
    [(instr (and op (or 'negq 'setl 'sete)) a)
     (fprintf port "  ~a " op)
     (write-arg port a)
     (newline port)]))

(: write-arg (-> Output-Port Any Void))
(define (write-arg port a)
  (match a
    [`(var ,x)
     (fprintf port "~a" x)]
    [`(reg ,r)
     (fprintf port "%~a" r)]
    [`(bytereg ,r)
     (fprintf port "%~a" r)]
    [`(deref ,r ,n)
     (fprintf port "~a(%~a)" n r)]
    [`(int ,n)
     (fprintf port "$~a" n)]))

;;; R

(: write-R (-> Any Output-Port Any Void))
(define (write-R port self)
  (match self
    [`(program ,_ ,code)
     (fprintf port ";; R\n")
     (parameterize ([pretty-print-columns 20])
       (pretty-write code port))]))
