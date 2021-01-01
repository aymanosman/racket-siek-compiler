#lang racket

(provide define-interp-test-suite
         define-extended-interp-test-suite)

(require rackunit
         siek
         (for-syntax siek/inspect
                     racket/match
                     racket/format
                     racket/list
                     racket/syntax
                     syntax/parse))

(define-syntax (define-interp-test-suite stx)
  (syntax-case stx ()
    [(_ id #:interpreter interp case* ...)
     (with-syntax ([id-info (format-id #'id "~a-info" (syntax-e #'id))])
       #`(begin
           (define-syntax id-info (hash 'test-cases '(case* ...)))
           (define-test-suite id
             (let-syntax ([pp (preprocess #'interp)])
               (pp case*)) ...)))]))

(define-syntax (define-extended-interp-test-suite stx)
  (syntax-case stx (test)
    [(_ id parent #:interpreter interp case* ...)
     (with-syntax ([(case* ...)
                    (append (syntax->list #'(case* ...))
                            (parent-test-cases #'parent))])
       #'(define-interp-test-suite id #:interpreter interp case* ...))]))

(define-for-syntax ((preprocess interp-stx) stx)
  (cond
    [(or (free-identifier=? interp-stx #'interp-C0)
         (free-identifier=? interp-stx #'interp-C1))
     ((preprocess-C interp-stx) stx)]
    [(or (free-identifier=? interp-stx #'interp-R0)
         (free-identifier=? interp-stx #'interp-R1)
         (free-identifier=? interp-stx #'interp-R2))
     ((preprocess-R interp-stx) stx)]
    [else
     (raise-syntax-error 'preprocess "unrecognised interpreter" interp-stx)]))

(define-for-syntax ((preprocess-C interp-stx) stx)
  (syntax-parse stx
    #:datum-literals (test)
    [(loc (test (~optional (~seq #:input input:str))
                (~seq #:expect result)
                stanza*
                ...))
     (define body->code stanza*->code)
     (define program->name (lambda (prog) (~a (C prog))))
     (with-syntax ([interp interp-stx]
                   [prog `(program () ,(body->code (syntax->datum #'(stanza* ...))))])
       (with-syntax ([name (program->name (syntax->datum #'prog))])
         #`(test-case name
             #,(syntax/loc #'loc
                 (check-interp interp 'prog result (~? input ""))))))]))

(define-for-syntax ((preprocess-R interp-stx) stx)
  (syntax-parse stx
    #:datum-literals (<=)
    [(loc [e <= input])
     (with-syntax ([interp interp-stx])
       #`(test-case (format "~v" 'e)
           #,(syntax/loc #'loc
               (check-interp
                interp
                '(program () e)
                (parameterize ([current-input-port (open-input-string input)])
                  e)
                input))))]
    [(loc e)
     ((preprocess interp-stx) #'(loc [e <= ""]))]))

(define-for-syntax (parent-test-cases parent-stx)
  (local-require threading)
  (cond
    [(syntax-e parent-stx)
     (~> (format-id parent-stx "~a-info" (syntax-e parent-stx))
         (syntax-local-value _)
         (hash-ref _ 'test-cases))]
    [else
     '()]))

(begin-for-syntax
  (define (stanza*->code _)
    (map stanza->code _))

  (define stanza->code
    (match-lambda
      [(list* label tail)
       (cons label
             (let loop ([tail tail])
               (cond
                 [(empty? (rest tail)) (first tail)]
                 [else
                  `(seq ,(first tail) ,(loop (rest tail)))])))])))

(define-check (check-interp interp p expected input)
  (define actual
    (parameterize ([current-input-port (open-input-string input)])
      (interp p)))
  (unless (equal? actual expected)
    (with-check-info (['actual actual]
                      ['expected expected])
      (fail-check))))
