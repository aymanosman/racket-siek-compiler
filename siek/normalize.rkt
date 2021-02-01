#lang racket

(provide normalize-R1
         normalize-R2)

(require (for-syntax "catamorphism.rkt")
         "gensym.rkt"
         "options.rkt"
         "raise-mismatch-error.rkt")

(define (normalize-R1 p)
  (send (new normalize-R1%) normalize p))

(define normalize-R1%
  (class object%
    (super-new)

    (define/public (who)
      'normalize-R1)

    (define/public (normalize p)
      (match p
        [`(program ,info ,e)
         `(program ,info ,(let-values ([(e env) (normalize-exp e)])
                            (unfold-env env e)))]
        [_
         (raise-mismatch-error (who) 'top p)]))

    (define/public (normalize-exp e)

      (define-match-expander prim
        (lambda (stx)
          (prim-cata #'normalize-arg stx)))

      (match e
        ;; R0
        [(? fixnum?)
         (values e empty)]
        [`(read)
         (values e empty)]
        [(prim '- (x env))
         (values `(- ,x) env)]
        [(prim '+ (x0 env0) (x1 env1))
         (values `(+ ,x0 ,x1) (append env0 env1))]
        ;; R1
        [(? symbol?) e]
        [`(let ([,x ,e0]) ,e1)
         (let-values ([(e0 env0) (normalize-exp e0)]
                      [(e1 env1) (normalize-exp e1)])
           (values `(let ([,x ,(unfold-env env0 e0)]) ,(unfold-env env1 e1))
                   empty))]
        [_
         (raise-mismatch-error (who) 'exp e)]))

    (define/public (normalize-arg e)

      (define-match-expander prim
        (lambda (stx)
          (prim-cata #'normalize-arg stx)))

      (match e
        ;; R0
        [(? fixnum?)
         (values e '())]
        [(prim 'read)
         (extend-env '() (fresh) '(read))]
        [(prim '- (x0 env0))
         (extend-env env0 (fresh) `(- ,x0))]
        [(prim '+ (x0 env0) (x1 env1))
         (extend-env (append env0 env1) (fresh) `(+ ,x0 ,x1))]
        ;; R1
        [(? symbol?)
         (values e empty)]
        [`(let ([,x ,e0]) ,e1)
         (define-values (v1 env1) (normalize-arg e1))
         (values v1 (append `((,x . ,(normalize-exp e0)))
                            env1))]
        [_
         (raise-mismatch-error (who) 'arg e)]))))

(define (extend-env env x e)
  (values x (append env `((,x . ,e)))))

(define (unfold-env env e)
  (cond
    [(empty? env) e]
    [else
     (match-define (cons x0 e0) (first env))
     `(let ([,x0 ,e0])
        ,(unfold-env (rest env) e))]))

(define (normalize-R2 p)
  (send (new normalize-R2%) normalize p))

(define normalize-R2%
  (class normalize-R1%
    (super-new)

    (define/override (who)
      'normalize-R2)

    (define/override (normalize-exp e)

      (define-match-expander prim
        (lambda (stx)
          (prim-cata #'normalize-arg stx)))

      (define-match-expander if
        (lambda (stx)
          (syntax-case stx ()
            [(_ (x ...) ...)
             #'(list 'if (app (lambda (e) (normalize-exp e)) x ...) ...)])))

      (match e
        [(? boolean?)
         (values e empty)]
        [(prim 'not (x env))
         (values `(not ,x) env)]
        [(prim (and op (or 'eq? '<)) (x0 env0) (x1 env1))
         (values `(,op ,x0 ,x1) (append env0 env1))]
        [(if (e0 env0) (e1 env1) (e2 env2))
         (values `(if ,e0 ,e1 ,e2) (append env0 env1 env2))]
        [_
         (super normalize-exp e)]))

    (define/override (normalize-arg e)

      (define-match-expander prim
        (lambda (stx)
          (prim-cata #'normalize-arg stx)))

      (define-match-expander if
        (lambda (stx)
          (syntax-case stx ()
            [(_ p (x* env*) ...)
             #'(list 'if p (app (lambda (e) (normalize-arg e)) x* env*) ...)])))

      (match e
        [(? boolean?)
         (values e '())]
        [(prim 'not (x0 env0))
         (extend-env env0 (fresh) `(not ,x0))]
        [(prim (and op (or 'eq? '<)) (x0 env0) (x1 env1))
         (extend-env (append env0 env1) (fresh) `(,op ,x0 ,x1))]
        [(if p (x0 env0) (x1 env1))
         (extend-env (append env0 env1) (fresh) `(if ,p ,x0 ,x1))]
        [_
         (super normalize-arg e)]))))
