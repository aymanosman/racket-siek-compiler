#lang racket

(provide instr*->locals)

(define (instr*->locals i*)
  (let loop ([i* i*]
             [locals (set)])
    (match i*
      ['() locals]
      [(cons i i*)
       (define l
         (match i
           [`(,_ (var ,a) (var ,b))
            (set a b)]
           [`(,_ ,_ (var ,a))
            (set a)]
           [`(,op (var ,a))
            (set a)]
           [_ #f]))
       (loop i* (cond
                  [l (set-union locals l)]
                  [else locals]))])))
