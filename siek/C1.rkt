#lang racket

(provide C1%
         interp-C1)

(require "C0.rkt")

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

    (define (op? op)
      (and (member op '(eq? < <= > >= not and or)) #t))

    (define/override (interp-tail code env t)
      (match t
        [`(goto ,l)
         (interp-tail code env (dict-ref code l))]
        [`(if (,(? op? op) ,a* ...) (goto ,then) (goto ,else))
         (match (interp-prim op (map (lambda (a) (interp-atom env a)) a*))
           [#t (interp-tail code env (dict-ref code then))]
           [#f (interp-tail code env (dict-ref code else))])]
        [_ (super interp-tail code env t)]))

    (define/override (interp-exp env e)
      (match e
        [`(not ,a)
         (not (interp-atom env a))]
        [`(,(? op? op) ,a* ...)
         (interp-prim op (map (lambda (a) (interp-atom env a)) a*))]
        [_ (super interp-exp env e)]))

    (define/public (cmp? c)
      (and (member c '(eq? < <= > >=))))

    (define/public (interp-prim op a*)
      (define proc
        (case op
          [(eq?) =]
          [(<) <]
          [(<=) <=]
          [(>) >]
          [(>=) >=]
          [(not) not]
          [(and) (lambda (a b) (and a b))]
          [(or) (lambda (a b) (or a b))]
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
