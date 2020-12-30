#lang racket

(provide typecheck-R2%
         typecheck-R2)

(require racket/control
         "typecheck-R1.rkt"
         "options.rkt")

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
      (hash 'eq?
            '(-> Boolean Boolean Boolean)
            '<
            '(-> Integer Integer Boolean)
            '<=
            '(-> Integer Integer Boolean)
            '>
            '(-> Integer Integer Boolean)
            '>=
            '(-> Integer Integer Boolean)
            'and
            '(-> Boolean Boolean Boolean)
            'or
            '(-> Boolean Boolean Boolean)
            'not
            '(-> Boolean Boolean)
            '-
            '(U
              (-> Integer Integer)
              (-> Integer Integer Integer))
            'read
            '(-> Integer)
            '+
            '(-> Integer Integer Integer)))

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
         (expect-type=? then else)]
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
                        '(U Boolean Integer))]
        [`(-> ,I* ... ,O)
         (cond
           [(= (length I*) (length t*))
            (for ([I I*]
                  [t t*])
              (expect-type=? t I))
            O]
           [else
            (fail-typecheck (who) "expected" (length I*) "given" (length t*))
            (abort #f)])]))))
