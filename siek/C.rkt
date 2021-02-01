#lang racket

(provide interp-C0
         interp-C1)

(require racket/fixnum
         "raise-mismatch-error.rkt")

#;
(define-language C0
  (terminals
   (fixnum? n)
   (symbol? x))
  (grammar
   (tail t := (return e) (seq s t))
   (stmt s := (assign x e))
   (exp e := a (read) (- a) (+ a a))
   (atom a := n x)))

(define (interp-C0 p)
  (send (new C0%) interp p))

(define C0%
  (class object%
    (super-new)

    (define/public (who)
      'interp-C0)

    (define/public (interp p)
      (match p
        [`(program ,_ ,code)
         (interp-tail code '() (dict-ref code 'start))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (interp-tail code env t)
      (match t
        [`(return ,e) (interp-exp env e)]
        [`(seq ,s ,t) (interp-tail code (interp-stmt env s) t)]
        [_
         (raise-mismatch-error (who) 'tail t)]))

    (define/public (interp-stmt env s)
      (match s
        [`(assign ,v ,e) (dict-set env v (interp-exp env e))]
        [_
         (raise-mismatch-error (who) 'stmt s)]))

    (define/public (interp-exp env e)
      (match e
        [(? atom?) (interp-atom env e)]
        [`(- ,a)
         #:when
         (atom? a)
         (fx- 0 (interp-exp env a))]
        [`(+ ,a0 ,a1)
         #:when
         (and (atom? a0) (atom? a1))
         (fx+ (interp-exp env a0) (interp-exp env a1))]
        [`(read)
         (match (read)
           [(? fixnum? r) r]
           [other
            (raise-argument-error (who) "fixnum?" other)])]
        [_
         (raise-mismatch-error (who) 'exp e)]))

    (define/public (interp-atom env a)
      (match a
        [(? fixnum?) a]
        [(? symbol?) (dict-ref env a)]
        [_
         (raise-mismatch-error (who) 'atom a)]))

    (define/public (atom? a)
      (match a
        [(? fixnum?) #t]
        [(? symbol?) #t]
        [_ #f]))))

#;
(define-language C1
  (terminals
   (boolean? b))
  (grammar
   (tail t := ...
              (goto l)
              (if (c a a) (goto l) (goto l)))
   (exp e := ...
             (not a) (c a a))
   (cmp c := eq? <)
   (atom a := ... b)))

(define (interp-C1 p)
  (send (new C1%) interp p))

(define C1%
  (class C0%
    (super-new)

    (define/override (who)
      'interp-C1)

    (define/override (interp-tail code env t)
      (match t
        [`(goto ,l)
         (interp-tail code env (dict-ref code l))]
        [`(if ,e (goto ,then) (goto ,else))
         (match (interp-exp env e)
           [#t (interp-tail code env (dict-ref code then))]
           [#f (interp-tail code env (dict-ref code else))])]
        [_ (super interp-tail code env t)]))

    (define/override (interp-exp env e)

      (define-match-expander prim
        (lambda (stx)
          (syntax-case stx ()
            [(_ op a* ...)
             #'(list op (app (lambda (a) (interp-atom env a)) a*) ...)])))

      (match e
        [(prim (and 'not op) a)
         (interp-prim op (list a))]
        [(prim (and (or 'eq? '<) op) a0 a1)
         (interp-prim op (list a0 a1))]
        [_ (super interp-exp env e)]))

    (define/public (interp-prim op a*)
      (define proc
        (case op
          [(eq?) =]
          [(<) <]
          [(not) not]
          [else
           (raise-arguments-error (who) "unrecognised operator"
                                  "operator" op)]))
      (apply proc a*))

    (define/override (interp-atom env a)
      (match a
        [(? boolean?) a]
        [_ (super interp-atom env a)]))

    (define/override (atom? a)
      (match a
        [(? boolean?) #t]
        [_ (super atom? a)]))))
