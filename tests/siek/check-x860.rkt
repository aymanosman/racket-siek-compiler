#lang racket/base

(provide check-x860?
         check-not-x860?
         check-x860=?
         check-x860*?
         check-not-x860*?
         check-x860*=?)

(require racket/list
         racket/match
         racket/bool)

(require siek)

(require rackunit)

(define (do-check-pred2 pred? ok? p)
  (define errors (get-errors pred? p))
  (unless (ok? errors)
    (fail-check/errors errors)))

(define-check (check-x860? p)
  (do-check-pred2 x860? empty? p))

(define-check (check-not-x860? p)
  (do-check-pred2 x860? pair? p))

(define-check (check-x860*? p)
  (do-check-pred2 x860*? empty? p))

(define-check (check-not-x860*? p)
  (do-check-pred2 x860*? pair? p))

(define-check (check-x860=? p0 p1)
  (check-x860? p0)
  (check-x860? p1)
  (unless (equal? p0 p1)
    (fail-check)))

(define-check (check-x860*=? p0 p1)
  (check-x860*? p0)
  (check-x860*? p1)
  (unless (equal? p0 p1)
    (fail-check)))

(define (get-errors pred? p)
  (define errors '())
  (parameterize ([current-mismatch-handler
                  (lambda (who kind term)
                    (cond
                      [(symbol=? 'errors kind)
                       (set! errors term)]
                      [else
                       (set! errors (list (cons kind term)))]))])
    (pred? p)
    errors))

(define (fail-check/errors errors)
  (with-check-info (['failed-matches (nested-info (map (match-lambda [(cons kind term) (make-check-info kind term)])
                                                       errors))])
    (fail-check)))
