#lang racket

(require rackunit)

(require siek-compiler/select-instructions-pass-R1)

(check-equal?
 (select-instructions-pass-R1
  '(program ()
            ((start . (seq
                       (assign x (+ 10 32))
                       (return x))))))
 '(program ()
           ((start . (block ()
                            (movq (int 10) (var x))
                            (addq (int 32) (var x))
                            (movq (var x) (reg rax)))))))

(check-equal?
 (select-instructions-pass-R1
  '(program ()
            ((start . (seq
                       (assign x (+ 10 x))
                       (return x))))))
 '(program ()
           ((start . (block ()
                            (addq (int 10) (var x))
                            (movq (var x) (reg rax)))))))

(check-equal?
 (select-instructions-pass-R1
  '(program ()
            ((start . (seq
                       (assign x (read))
                       (return x))))))
 '(program ()
           ((start . (block ()
                            (callq read_int)
                            ;; TODO redundant
                            (movq (reg rax) (var x))
                            (movq (var x) (reg rax)))))))

(check-equal?
 (select-instructions-pass-R1
  '(program ()
            ((start . (seq
                       (assign y 42)
                       (return y))))))
 '(program ()
           ((start . (block ()
                            ;; TODO redundant
                            (movq (int 42) (var y))
                            (movq (var y) (reg rax)))))))

(check-equal?
 (select-instructions-pass-R1
  '(program ()
            ((start . (seq (assign y (+ 10 20))
                           (return (- y)))))))

 '(program ()
           ((start . (block ()
                            (movq (int 10) (var y))
                            (addq (int 20) (var y))
                            (movq (var y) (reg rax))
                            (negq (reg rax)))))))

(check-equal?
 (select-instructions-pass-R1
  '(program ()
            ((start . (seq (assign y (+ y 20))
                           (return (- y)))))))

 '(program ()
           ((start . (block ()
                            (addq (int 20) (var y))
                            (movq (var y) (reg rax))
                            (negq (reg rax)))))))

(check-equal?
 (select-instructions-pass-R1
  '(program ()
            ((start . (seq (assign y (+ y 20))
                           (return (- y)))))))

 '(program ()
           ((start . (block ()
                            (addq (int 20) (var y))
                            (movq (var y) (reg rax))
                            (negq (reg rax)))))))

(check-equal?
 (select-instructions-pass-R1
  '(program ()
            ((start . (seq (assign x 10)
                           (seq
                            (assign y (+ x 20))
                            (return y)))))))

 '(program ()
           ((start . (block ()
                            (movq (int 10) (var x))
                            (movq (var x) (var y))
                            (addq (int 20) (var y))
                            (movq (var y) (reg rax)))))))
