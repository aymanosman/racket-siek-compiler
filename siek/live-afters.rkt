#lang racket

(provide live-afters)

(require "instr.rkt")

(define live-afters
  (case-lambda
   [(instr*)
    (rest (live-afters (list (set)) (reverse instr*)))]
   ;; L(k) = L(k+1) - W(k) + R(k)
   [(acc i*)
    (match i*
      ['() acc]
      [(cons i i*)
       (define next
         (set-union (set-subtract (first acc) (instr->writes i)) (instr->reads i)))
       (live-afters (cons next acc) i*)])]))

(module+ test
  (require rackunit)

  (check-equal?
   (live-afters (list (set))
                '((jmp conclusion)))
   (list (set 'rax 'rsp)
         (set)))

  (check-equal?
   (live-afters (list (set 'w 'y 'z))
                '((movq (var x) (var z))))
   (list (set 'w 'x 'y)
         (set 'w 'y 'z)))

  (check-equal?
   (live-afters (list (set 't.1))
                '((addq (var t.2) (var t.1))))
   (list (set 't.1 't.2)
         (set 't.1)))

  (check-equal?
   (live-afters (list (set 't.1))
                '((negq (var t.1))))
   (list (set 't.1)
         (set 't.1)))

  (check-equal?
   (live-afters (list (set 't.1))
                '((movq (var t.2) (var t.1))))
   (list (set 't.2)
         (set 't.1)))

  (check-equal?
   (live-afters
    '((movq (int 1) (var v))
      (movq (int 42) (var w))
      (movq (var v) (var x))
      (addq (int 7) (var x))
      (movq (var x) (var y))
      (movq (var x) (var z))
      (addq (var w) (var z))
      (movq (var y) (var t))
      (negq (var t))
      (movq (var z) (reg rax))
      (addq (var t) (reg rax))
      (jmp conclusion)))
   (list (set 'v 'rsp)
         (set 'v 'w 'rsp)
         (set 'w 'x 'rsp)
         (set 'w 'x 'rsp)
         (set 'w 'x 'y 'rsp)
         (set 'w 'y 'z 'rsp)
         (set 'z 'y 'rsp)
         (set 't 'z 'rsp)
         (set 't 'z 'rsp)
         (set 'rax 't 'rsp)
         (set 'rax 'rsp)
         (set))))
