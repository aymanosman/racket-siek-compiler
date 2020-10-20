#lang racket

(require rackunit)

(require siek)

(check-equal?
 (uncover-live-pass-R1
  '(program ()
     ((start . (block ()
                 (movq (int 1) (var v))
                 (movq (int 46) (var w))
                 (movq (var v) (var x))
                 (addq (int 7) (var x))
                 (movq (var x) (var y))
                 (addq (int 4) (var y))
                 (movq (var x) (var z))
                 (addq (var w) (var z))
                 (movq (var y) (var t.1))
                 (negq (var t.1))
                 (movq (var z) (reg rax))
                 (addq (var t.1) (reg rax))
                 (jmp conclusion))))))

 `(program ()
    ((start . (block ((live-afters . ,(list (set)
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
                                            (set))))
                (movq (int 1) (var v))
                (movq (int 46) (var w))
                (movq (var v) (var x))
                (addq (int 7) (var x))
                (movq (var x) (var y))
                (addq (int 4) (var y))
                (movq (var x) (var z))
                (addq (var w) (var z))
                (movq (var y) (var t.1))
                (negq (var t.1))
                (movq (var z) (reg rax))
                (addq (var t.1) (reg rax))
                (jmp conclusion))))))