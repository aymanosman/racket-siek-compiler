#lang racket

(provide assign-homes-pass-R1)

(define (assign-homes-pass-R1 p)
  (match p
    [`(program ,info ((start . ,block)))
     (define-values (env stack-space) (locals->homes (info-ref info 'locals)))
     `(program ((stack-space . ,stack-space))
        ((start . ,(assign-homes-block env block))))]
    [_
     (report-mismatch-error 'top p)]))

(define (assign-homes-block env b)
  (match b
    [`(block ,info ,instr* ...)
     `(block ,info ,@(assign-homes-instr* env instr*))]
    [_
     (report-mismatch-error 'block b)]))

(define (assign-homes-instr* env i*)
  (map (lambda (i) (assign-homes-instr env i)) i*))

(define (assign-homes-instr env i)
  (match i
    [`(jmp ,l) i]
    [`(callq ,l) i]
    [`(,x86-operator ,a* ...)
     `(,x86-operator ,@(map (lambda (a) (assign-homes-arg env a)) a*))]
    [_
     (report-mismatch-error 'instr i)]))

(define (assign-homes-arg env a)
  (match a
    [`(int ,_) a]
    [`(reg ,_) a]
    [`(var ,v) `(deref rbp ,(lookup env v))]
    [_
     (report-mismatch-error 'arg a)]))

;; Aux

(define (lookup env var)
  (match (assoc var env)
    [(cons _ value) value]
    [_
     (raise-arguments-error var "undefined variable")]))

(define (locals->homes var*)
  (let loop ([env '()]
             [v var*]
             [l 0])
    (cond
      [(empty? v)
       (values env (abs l))]
      [else
       (define r (- l 8))
       (loop (cons (cons (first v) r) env)
             (rest v)
             r)])))

(define (info-ref info key)
  (match (assoc key info)
    [(cons _ var*) var*]
    [else
     (raise-arguments-error 'info-ref "no value found for key"
                            "key" key)]))

(define (report-mismatch-error kind term)
  (raise-arguments-error 'assign-homes-pass-R1 "failed match"
                         "kind" kind
                         "term" term))
