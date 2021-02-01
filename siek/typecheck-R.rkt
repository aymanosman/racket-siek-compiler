#lang racket

(provide typecheck-R0
         typecheck-R1
         typecheck-R2)

(require racket/control
         "options.rkt"
         "raise-mismatch-error.rkt")

(define (typecheck-R0 p)
  (send (new typecheck-R0%) typecheck p))

(define typecheck-R0%
  (class object%
    (super-new)

    (define/public (who)
      'typecheck-R0)

    (define/public (typecheck p)
      (match p
        [`(program ,info ,e)
         (define-values (errors type)
           (collect-type-errors
            (expect-type=? ((typecheck-exp '()) e) 'Integer e)))
         (define new-info
           (cond
             [(empty? errors)
              (dict-set info 'type type)]
             [else
              (when (compiler-raise-exception-on-type-error)
                (raise-arguments-error (who) "failed typecheck"
                                       "type-errors" errors))
              (dict-set info 'type-errors errors)]))
         `(program ,new-info ,e)]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public ((typecheck-exp env) e)
      (match e
        [(? fixnum? n) 'Integer]
        [`(read) 'Integer]
        [`(- ,e)
         (expect-type env e 'Integer)]
        [`(+ ,e0 ,e1)
         (expect-type env e0 'Integer)
         (expect-type env e1 'Integer)]
        [_
         (raise-mismatch-error (who) 'exp e)]))

    (define/public (type=? t1 t2)
      (equal? t1 t2))

    (define/public (fail-typecheck who . args)
      (define err (current-type-errors))
      (cond
        [(box? err)
         (set-box! err (append (unbox err) args))]
        [else
         (apply raise-arguments-error
                (who)
                "called outside of (collect-type-errors) context"
                args)]))

    (define/public (expect-type=? t1 t2 [ctx #f])
      (unless (type=? t1 t2)
        (fail-typecheck (who) "expected" t2 "given" t1 "context" ctx)
        (abort #f))
      t1)

    (define/public (expect-type env e t)
      (expect-type=? ((typecheck-exp env) e) t e))

    (define-syntax-rule (collect-type-errors body)
      (parameterize ([current-type-errors (box '())])
        (let ([t (prompt body)])
          (values (unbox (current-type-errors)) t))))

    (define/public (report-type-errors errors)
      (apply raise-arguments-error
             (who)
             "type mismatch"
             errors))))

(define (typecheck-R1 p)
  (send (new typecheck-R1%) typecheck p))

(define typecheck-R1%
  (class typecheck-R0%
    (super-new)

    (inherit expect-type)

    (define/override (who)
      'typecheck-R1)

    (define/override ((typecheck-exp env) e)
      (match e
        [(? symbol? x)
         (dict-ref env x)]
        [`(let ([,x ,e0]) ,e1)
         ((typecheck-exp (dict-set env x (expect-type env e0 'Integer)))
          e1)]
        [_
         ((super typecheck-exp env) e)]))))

(define (typecheck-R2 p)
  (send (new typecheck-R2%) typecheck p))

(define typecheck-R2%
  (class typecheck-R1%
    (super-new)

    (inherit expect-type expect-type=? fail-typecheck)

    (define/override (who)
      'typecheck-R2)

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
      (hash 'eq? '(-> Integer Integer Boolean)
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
                  (raise-arguments-error (who)
                                         "no type for operator"
                                         "operator"
                                         op))))

    (define/override ((typecheck-exp env) e)
      (match e
        [(? boolean?) 'Boolean]
        [`(if ,e0 ,e1 ,e2)
         (expect-type env e0 'Boolean)
         (define then ((typecheck-exp env) e1))
         (define else ((typecheck-exp env) e2))
         (expect-type=? then else e)]
        [`(,op ,e* ...)
         #:when
         (op? op)
         (typecheck-op (operator-type op) (map (typecheck-exp env) e*))]
        [_
         ((super typecheck-exp env) e)]))

    ;; TODO duplicated
    (define/public (op? v)
      (member v '(eq? < <= > >= and or not - read +)))

    (define/public (typecheck-op op t*)
      (match op
        [`(U ,t1 ,t2)
         (expect-type=? (or
                         ;; FIXME isolate failures
                         (parameterize ([current-type-errors (box '())])
                           (prompt (typecheck-op t1 t*)))
                         (prompt (typecheck-op t2 t*)))
                        '(U Boolean Integer)
                        (list* op t*))]
        [`(-> ,I* ... ,O)
         (cond
           [(= (length I*) (length t*))
            (for ([I I*]
                  [t t*])
              (expect-type=? t I (list* op t*)))
            O]
           [else
            (fail-typecheck (who) "expected" (length I*) "given" (length t*) "context" (list* op t*))
            (abort #f)])]))))
