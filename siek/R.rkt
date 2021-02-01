#lang racket

(provide interp-R0
         interp-R0†
         interp-R1
         interp-R1†
         interp-R2
         interp-R2†)

(require "raise-mismatch-error.rkt")

;; exp := int | (read) | (- e) | (+ e e)

(define (interp-R0 p)
  (send (new R0%) interp p))

(define R0%
  (class object%
    (super-new)

    (define/public (who)
      'interp-R0)

    (define/public (interp p)
      (match p
        [`(program ,_ ,e)
         ((interp-exp '()) e)]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public ((interp-exp env) v)
      (match v
        [(? fixnum? n) n]
        [`(read)
         (interp-read)]
        [`(- ,e)
         (- ((interp-exp env) e))]
        [`(+ ,e0 ,e1)
         (+ ((interp-exp env) e0) ((interp-exp env) e1))]
        [_
         (raise-mismatch-error (who) 'exp v)]))

    (define/public (interp-read)
      (match (read)
        [(? fixnum? n) n]
        [other
         (raise-argument-error (who) "fixnum?" other)]))))

;; exp := a | (read)
;;      | (- a) | (+ a a)
;; atom := n

(define (interp-R0† p)
  (send (new R0†%) interp p))

(define R0†%
  (class R0%
    (super-new)

    (inherit interp-read)

    (define/override (who)
      'interp-R0†)

    (define/override (interp p)
      (match p
        [`(program ,_ ,e)
         ((interp-exp '()) e)]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/override ((interp-exp env) e)
      (match e
        [(? atom? a)
         a]
        [`(read)
         (interp-read)]
        [`(- ,a)
         (- ((interp-atom env) a))]
        [`(+ ,a0 ,a1)
         (+ ((interp-atom env) a0) ((interp-atom env) a1))]
        [_
         (raise-mismatch-error (who) 'exp e)]))

    (define/public ((interp-atom env) a)
      (match a
        [(? fixnum?) a]
        [_ (raise-mismatch-error (who) 'atom a)]))

    (define/public (atom? a)
      (match a
        [(? fixnum?) #t]
        [_ #f]))))

;; exp := ...
;;      | var | (let ([x e]) e)

(define (interp-R1 p)
  (send (new R1%) interp p))

(define R1%
  (class R0%
    (super-new)

    (define/override (who)
      'interp-R1)

    (define/override ((interp-exp env) e)
      (match e
        [(? symbol? x)
         (dict-ref env x)]
        [`(let ([,x ,e0]) ,e1)
         ((interp-exp (dict-set env x ((interp-exp env) e0)))
          e1)]
        [_
         ((super interp-exp env) e)]))))

;; exp := ...
;;      | (let ([x e]) e)
;; atom := ... | x

(define (interp-R1† p)
  (send (new R1†%) interp p))

(define R1†%
  (class R0†%
    (super-new)

    (define/override (who)
      'interp-R1†)

    (define/override ((interp-exp env) e)
      (match e
        [(? symbol? x)
         (dict-ref env x)]
        [`(let ([,x ,e0]) ,e1)
         (define new-env
           (dict-set env x ((interp-exp env) e0)))
         ((interp-exp new-env) e1)]
        [_ ((super interp-exp env) e)]))

    (define/override ((interp-atom env) a)
      (match a
        [(? symbol?)
         (dict-ref env a)]
        [_ ((super interp-atom env) a)]))

    (define/override (atom? a)
      (match a
        [(? symbol?) #t]
        [_ (super atom? a)]))))

#;
(define-language R2
  (terminals
   (boolean? b))
  (grammar
   (exp e := ...
             (- e e)
             b
             (and e e) (or e e) (not e)
             (c e e) (if e e e))
   (cmp c := eq? < <= > >=)))

(define (interp-R2 p)
  (send (new R2%) interp p))

(define R2%
  (class R1%
    (super-new)

    (define/override (who)
      'interp-R2)

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
            (raise-mismatch-error (who) 'exp cond)])]
        [`(and ,e0 ,e1)
         (match ((interp-exp env) e0)
           [#t ((interp-exp env) e1)]
           [#f #f])]
        [`(or ,e0 ,e1)
         (match ((interp-exp env) e0)
           [#f ((interp-exp env) e1)]
           [#t #t])]
        [`(,op ,e* ...)
         #:when
         (op? op)
         (interp-op op (map (interp-exp env) e*))]
        [_
         ((super interp-exp env) e)]))

    (define/public (op? v)
      (member v '(eq? < <= > >= and or not - read +)))

    (define/public (operator-implementation)
      (hash 'eq?
            eq?
            '<
            <
            '<=
            <=
            '>
            >
            '>=
            >=
            'and
            and-fn
            'or
            or-fn
            'not
            not
            '-
            -
            'read
            read
            '+
            +))

    (define/public (interp-op op args)
      (apply (hash-ref (operator-implementation) op) args))))

(define (and-fn a b)
  (and a b))

(define (or-fn a b)
  (or a b))

(define (cmp? v)
  (member v '(eq? <)))

#;
(define-extended-language R2† R1†
  (grammar
   (exp e := ...
             (not a) (c a a) (if e e e))
   (atom a := ... b)))

(define (interp-R2† p)
  (send (new R2†%) interp p))

(define R2†%
  (class R1†%
    (super-new)

    (define/override (who)
      'interp-R2†)

    (define/override ((interp-exp env) e)
      (match e
        [`(- ,a0 ,a1)
         (- ((interp-atom env) a0)
            ((interp-atom env) a1))]
        [`(not ,a)
         (not ((interp-atom env) a))]
        [`(,c ,a0 ,a1)
         #:when (cmp? c)
         (define op
           (case c
             [(eq?) =]
             [(<) <]
             [else (raise-arguments-error (who) "impossible state")]))
         (op ((interp-atom env) a0)
             ((interp-atom env) a1))]
        [`(if ,e0 ,e1 ,e2)
         (match ((interp-exp env) e0)
           [#t ((interp-exp env) e1)]
           [#f ((interp-exp env) e2)])]
        [_
         ((super interp-exp env) e)]))

    (define/override ((interp-atom env) a)
      (match a
        [(? boolean?) a]
        [_
         ((super interp-atom env) a)]))

    (define/override (atom? a)
      (match a
        [(? boolean?) #t]
        [_
         (super atom? a)]))))
