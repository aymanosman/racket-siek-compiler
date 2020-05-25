#lang racket

(provide build-interference-pass-R1)

(require graph)

(define (build-interference-pass-R1 p)
  (match p
    [_
     (report-mismatch-error 'top p)]))

;; Aux

(define (report-mismatch-error kind term)
  (raise-arguments-error 'build-interference-pass-R1 "failed match"
                         "kind" kind
                         "term" term))

'((v w)
  (w x y z)
  (t.1 z))

(define g
  (unweighted-graph/undirected '((a b) (c d))))

(get-edges g)

(define live-afters
  (list (set)
        (set 'v)
        (set 'v 'w)
        (set 'w 'x)
        (set 'w 'x)
        (set 'w 'x 'y)
        (set 'w 'x 'y)
        (set 'w 'y 'z)
        (set 'y 'z)
        (set 'z 't.1)
        (set 'z 't.1)
        (set 't.1)
        (set)
        (set)))

;; naive
(let loop ([l* (for/list ([x  live-afters]
                          [y  (rest live-afters)])
                 (cons x y))]
           [acc '()])
  (match l*
    [(cons (cons a b) l*)
     (printf "a: ~a\n" a)
     (printf "b: ~a\n" b)
     (loop l* acc)]
    [_
     acc]))
