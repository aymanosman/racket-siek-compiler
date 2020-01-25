#lang racket

(provide select-instructions-pass-R1)

(define (select-instructions-pass-R1 p)
  (match p
    [`(program ,info ((start . ,tail)))
     `(program ,info
               ((start . (block ()
                           ,@(select-instructions-tail tail)
                           (jmp conclusion)))))]
    [_
     ((current-R1-mismatch-handler) 'top p)]))

(define (select-instructions-tail t)
  (match t
    [`(return ,e)
     (select-instructions-stmt `(assign (reg rax) ,e))]
    [`(seq ,stmt ,tail)
     (append (select-instructions-stmt stmt)
             (select-instructions-tail tail))]
    [_
     ((current-R1-mismatch-handler) 'tail t)]))

(define (select-instructions-stmt stmt)
  (match stmt
    [`(assign ,v ,e)
     (define r
       (match v
         [`(reg rax) v]
         [(? symbol?) `(var ,v)]
         [_
          ((current-R1-mismatch-handler) 'var v)]))
     (match e
       ;; Arg
       [(? fixnum?) (list `(movq (int ,e) ,r))]
       [(? symbol?) (list `(movq (var ,e) ,r))]
       ;; read
       [`(read)
        (cons `(callq read_int)
              (cond
                [(equal? '(reg rax) r)
                 '()]
                [else
                 (list `(movq (reg rax) ,r))]))]
       ;; -
       [`(- ,a)
        (list `(movq ,(select-instructions-arg a) ,r)
              `(negq ,r))]
       ;; +
       [`(+ ,(== v) ,a)
        (list `(addq ,(select-instructions-arg a) ,r))]
       [`(+ ,a ,(== v))
        (list `(addq ,(select-instructions-arg a) ,r))]
       [`(+ ,a0 ,a1)
        (list `(movq ,(select-instructions-arg a0) ,r)
              `(addq ,(select-instructions-arg a1) ,r))])]
    [_
     ((current-R1-mismatch-handler) 'stmt stmt)]))

(define (select-instructions-arg a)
  (match a
    [(? fixnum?) `(int ,a)]
    [(? symbol?) `(var ,a)]
    [_
     ((current-R1-mismatch-handler) 'arg a)]))

(define current-R1-mismatch-handler
  (make-parameter
   (lambda (kind term)
     (raise-arguments-error 'select-instructions-pass-R1 "failed match"
                            "kind" kind
                            "term" term))))
