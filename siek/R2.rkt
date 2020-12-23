#lang racket

(provide R2%
         R2?
         interp-R2
         typecheck-R2)

(require racket/control
         "R1.rkt"
         "options.rkt"
         "raise-mismatch-error.rkt")

;; bool := #t | #f
;; cmp := eq? | < | <= | > | >=
;; exp := ...
;;      | (- e e)
;;      | bool | (and e e) | (or e e) | (not e)
;;      | (cmp e e) | (if e e e)

(define (R2? v)
  (send (new R2%) ? v))

(define (interp-R2 p)
  (send (new R2%) interp p))

(define (typecheck-R2 p)
  (send (new R2%) typecheck p))

(define R2%
  (class R1%
    (super-new)

    (inherit expect-type expect-type=? fail-typecheck)

    (define/override (who-interp)
      'interp-R2)

    (define/override (who-typecheck)
      'typecheck-R2)

    (define/override (exp? e)
      (match e
        [`(- ,e0 ,e1)
         (and (exp? e0) (exp? e1))]
        [(? boolean?) #t]
        [`(if ,e0 ,e1 ,e2)
         (and (exp? e0) (exp? e1) (exp? e2))]
        [`(,op ,e* ...) #:when (op? op)
                        (andmap (lambda (e) (exp? e)) e*)]
        [_ (super exp? e)]))

    (define/override ((interp-exp env) e)
      (match e
        [`(- ,e0 ,e1)
         (- ((interp-exp env) e0) ((interp-exp env) e1))]
        [(? boolean?) e]
        [`(if ,cond ,then ,else)
         (match ((interp-exp env) cond)
           [#t ((interp-exp env) then)]
           [#f ((interp-exp env) else)]
           [_
            (raise-mismatch-error (who-interp) 'exp cond)])]
        [`(and ,e0 ,e1)
         (match ((interp-exp env) e0)
           [#t ((interp-exp env) e1)]
           [#f #f])]
        [`(or ,e0 ,e1)
         (match ((interp-exp env) e0)
           [#f ((interp-exp env) e1)]
           [#t #t])]
        [`(,op ,e* ...) #:when (op? op)
                        (interp-op op (map (interp-exp env) e*))]
        [_
         ((super interp-exp env) e)]))

    (define/public (op? v)
      (member v '(eq? < <= > >= and or not - read +)))

    (define/public (operator-implementation)
      (hash 'eq? eq?
            '< <
            '<= <=
            '> >
            '>= >=
            'and and-fn
            'or or-fn
            'not not
            '- -
            'read read
            '+ +))

    (define/public (interp-op op args)
      (apply (hash-ref (operator-implementation) op) args))

    (define/override (type=? t1 t2)
      (match* (t1 t2)
        [(`(U ,t11 ,t12) t2)
         (or (type=? t11 t2)
             (type=? t12 t2))]
        [(t1 `(U ,t21 ,t22))
         (or (type=? t1 t21)
             (type=? t1 t22))]
        [(t1 t2)
         (equal? t1 t2)]))

    (define/public (operator-types)
      (hash 'eq? '(-> Boolean Boolean Boolean)
             '< '(-> Integer Integer Boolean)
             '<= '(-> Integer Integer Boolean)
             '> '(-> Integer Integer Boolean)
             '>= '(-> Integer Integer Boolean)
             'and '(-> Boolean Boolean Boolean)
             'or '(-> Boolean Boolean Boolean)
             'not '(-> Boolean Boolean)
             '- '(U (-> Integer Integer)
                    (-> Integer Integer Integer))
             'read '(-> Integer)
             '+ '(-> Integer Integer Integer)))

    (define/public (operator-type op)
      (hash-ref (operator-types)
                op
                (lambda ()
                  (raise-arguments-error (who-typecheck) "no type for operator"
                                         "operator" op))))

    (define/override ((typecheck-exp env) e)
      (match e
        [(? boolean?) 'Boolean]
        [`(if ,e0 ,e1 ,e2)
         (expect-type env e0 'Boolean)
         (define then ((typecheck-exp env) e1))
         (define else ((typecheck-exp env) e2))
         (expect-type=? then else)]
        [`(,op ,e* ...) #:when (op? op)
                        (typecheck-op (operator-type op) (map (typecheck-exp env) e*))]
        [_
         ((super typecheck-exp env) e)]))

    (define/public (typecheck-op op t*)
      (match op
        [`(U ,t1 ,t2)
         (expect-type=?  (or
                          ;; FIXME isolate failures
                          (parameterize ([current-type-errors (box '())])
                            (prompt (typecheck-op t1 t*)))
                          (prompt (typecheck-op t2 t*)))
                         '(U Boolean Integer))]
        [`(-> ,I* ... ,O)
         (cond
           [(= (length I*) (length t*))
            (for ([n (in-naturals 1)]
                  [I I*]
                  [t t*])
              (expect-type=? t I))
            O]
           [else
            (fail-typecheck (who-typecheck) "expected" (length I*) "given" (length t*))
            (abort #f)])]))))

(define (and-fn a b)
  (and a b))

(define (or-fn a b)
  (or a b))
