#lang racket

(provide live-afters-tests)

(require rackunit
         siek/live-afters)

(module+ test
  (require rackunit/text-ui)
  (run-tests live-afters-tests))

(define-test-suite live-afters-tests
  (test-case "movq x z => {w,y,z} - {z} + {x}"
    (check-equal?
     (live-afters-instr* (list (set 'w 'y 'z))
                         empty
                         '((movq (var x) (var z))))
     (list (set 'w 'x 'y)
           (set 'w 'y 'z))))

  (test-case "addq x y => {v} - {y} + {x,y}"
    (check-equal?
     (live-afters-instr* (list (set 'v))
                         empty
                         '((addq (var x) (var y))))
     (list (set 'v 'x 'y)
           (set 'v))))

  (test-case "negq x => {v} - {x} + {x}"
    (check-equal?
     (live-afters-instr* (list (set 'v))
                         empty
                         '((negq (var x))))
     (list (set 'v 'x)
           (set 'v))))

  (test-case "movq 1 v => {v} - {v} + {}"
    (check-equal?
     (live-afters-instr* (list (set 'v))
                         empty
                         '((movq (int 1) (var v))))
     (list (set)
           (set 'v))))

  (test-case "callq read_int => {v} - {} + {}"
    (check-equal?
     (live-afters-instr* (list (set 'v))
                         empty
                         '((callq read_int)))
     (list (set 'v)
           (set 'v))))

  (test-case "jmp conclusion => {} - {} + {rax, rsp}"
    (check-equal?
     (live-afters-instr* (list (set))
                         empty
                         '((jmp conclusion)))
     (list (set 'rax 'rsp)
           (set))))

  (test-case "running example"
    (check-equal?
     (live-afters-instr*
      empty
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
     (list (set 'rsp)
           (set 'v 'rsp)
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
           (set)))))
