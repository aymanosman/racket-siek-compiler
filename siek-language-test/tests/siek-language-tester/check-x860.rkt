#lang racket/base

(provide check-x860?
         check-not-x860?
         check-x860*?
         check-not-x860*?)

(require racket/list
         racket/match
         racket/function)

(require rackunit
         siek-language/x860)

(define-check (check-x860? p)
  (define errors (check/errors p))
  (unless (empty? errors)
    (fail-check/errors errors)))

(define-check (check-not-x860? p)
  (define errors (check/errors p))
  (unless (not (empty? errors))
    (fail-check)))

(define-check (check-x860*? p)
  (define errors (parameterize ([current-x86? x860*?])
                   (check/errors p)))
  (unless (empty? errors)
    (fail-check/errors errors)))

(define-check (check-not-x860*? p)
  (define errors (parameterize ([current-x86? x860*?])
                   (check/errors p)))
  (unless (not (empty? errors))
    (fail-check)))

;; Aux

(define current-x86? (make-parameter x860?))

(define (check/errors p)
  (define errors '())
  (parameterize ([current-x860-mismatch-handler
                  (case-lambda
                    [(kind term)
                     (set! errors (list (cons kind term)))]
                    [(the-errors)
                     (set! errors the-errors)])])
    ((current-x86?) p)
    errors))

(define (fail-check/errors errors)
  (with-check-info*
    (map (match-lambda
           [(cons kind term)
            (make-check-info kind term)]) errors)
    (thunk (fail-check))))
