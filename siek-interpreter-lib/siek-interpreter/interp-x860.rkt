#lang racket

(provide interp-x860
         interp-x860*)

(require racket/undefined)

(require racket/fixnum)

(define current-x86 (make-parameter 'x860))

(define (interp-x860* p)
  (parameterize ([current-x86 'x860*])
    (interp-x860 p)))

(define (interp-x860 p)
  (lookup (interp-x860/env p) 'rax))

(define (interp-x860/env p)
  (match p
    [`(program ,info ,code0)

     (define stack-space
       (case (current-x86)
         [(x860*) 0]
         [else
          (info-ref info 'stack-space)]))

     (define dyld_stub_binder
       '(dyld_stub_binder . (block ())))

     (define conclusion
       `(conclusion . (block ()
                        (addq (int ,stack-space) (reg rsp))
                        (popq (reg rbp))
                        (retq))))
     (define main
       `(main . (block ()
                  (pushq (reg rbp))
                  (movq (reg rsp) (reg rbp))
                  (subq (int ,stack-space) (reg rsp))
                  (jmp start))))

     (define code (list* conclusion main dyld_stub_binder code0))
     (interp-block code
                   '((rsp . 0)
                     (rbp . 0)
                     (0 . dyld_stub_binder))
                   (code-ref code 'main))]
    [_
     (report-mismatch-error 'top p)]))

(define (interp-block code env b)
  (match b
    [`(block ,info ,instr* ...)
     (interp-instr* code env instr*)]
    [_
     (report-mismatch-error 'block b)]))

(define (interp-instr* code env i*)
  (match i*
    ['()
     env]
    [(cons i i*)
     (match i
       [`(jmp ,l)
        (interp-block code env (code-ref code l))]
       ['(retq)
        (define rsp (lookup env 'rsp))
        ;; TODO This should be a representation of a 'return address',
        ;; say (return-address label index)
        (define l (lookup env rsp))
        (interp-block code
                      (extend env (cons rsp (+ rsp 8)))
                      (code-ref code l))]
       [_
        (interp-instr* code (interp-instr env i) i*)])]))

(define (interp-instr env i)
  (match i
    [`(addq ,a0 ,a1)
     (extend env (cons (l-value a1)
                       (fx+ (r-value env a1) (r-value env a0))))]
    [`(subq ,a0 ,a1)
     (extend env (cons (l-value a1)
                       (fx- (r-value env a1) (r-value env a0))))]
    [`(movq ,a0 ,a1)
     (extend env (cons (l-value a1)
                       (r-value env a0)))]
    [`(negq ,a)
     (extend env (cons (l-value a) (fx- 0 (r-value env a))))]
    ;; callq
    [`(pushq ,a)
     (define rsp (- (lookup env 'rsp) 8))
     (extend env
             (cons rsp (r-value env a))
             (cons 'rsp rsp))]
    [`(popq ,a)
     (define rsp (lookup env 'rsp))
     (define val (lookup env rsp))
     (extend env
             (cons (l-value a) val)
             (cons rsp undefined)
             (cons 'rsp (+ rsp 8)))]
    [_
     (report-mismatch-error 'instr i)]))

;; Aux

(define (code-ref code label)
  (match (assoc label code)
    [(cons _ block)
     block]
    [_
     (report-missing-label-error label code)]))

(define (info-ref info key)
  (match (assoc key info)
    [(cons _ value)
     value]
    [_
     (report-missing-info-key-error key info)]))

(define (report-missing-label-error label code)
  (raise-arguments-error 'interp-x860 "missing label"
                         "label" label
                         "in labels..." (map car code)))

(define (report-missing-info-key-error key code)
  (raise-arguments-error 'interp-x860 "key missing in info"
                         "key" key))

(define (report-mismatch-error kind term)
  (raise-arguments-error 'interp-x860 "failed match"
                         "kind" kind
                         "term" term))

(define (extend env . entry*)
  (let loop ([env env]
             [entry* entry*])
    (cond
      [(empty? entry*) env]
      [else
       (loop
        (cons (first entry*) env)
        (rest entry*))])))

(define (lookup env loc)
  (match (assoc loc env)
    [(cons _ value) value]
    [_
     (raise-arguments-error 'interp-x860 "location read before being written"
                            "location" loc
                            "env..." env)]))

(define (l-value a)
  (match a
    [`(int ,_) (raise-arguments-error 'interp-x860 "invalid l-value"
                                      "arg" a)]
    [`(reg ,r) r]
    [`(deref ,reg ,m) m]
    [`(var ,v)
     (case (current-x86)
       [(x860*) v]
       [else (report-variables-not-supported-error v)])]
    [_
     (report-mismatch-error 'l-value a)]))

(define (r-value env a)
  (match a
    [`(int ,n) n]
    [`(reg ,r) (lookup env r)]
    [`(var ,v)
     (case (current-x86)
       [(x860*) (lookup env v)]
       [else (report-variables-not-supported-error v)])]
    [`(deref ,reg ,m) (lookup env m)]
    [_
     (report-mismatch-error 'r-value a)]))

(define (report-variables-not-supported-error v)
  (raise-arguments-error 'interp-x860 "variables are not supported in the current language (did you mean to use pseudo-x86?)"
                         "current-x86" (current-x86)
                         "variable" v))
