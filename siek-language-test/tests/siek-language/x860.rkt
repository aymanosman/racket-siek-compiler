#lang racket

(require rackunit)

(require tests/siek-language-tester/check-x860)

;; x86

(check-x860?
 '(program ()
    ((start . (block ()
                (movq (int 10) (reg rax)))))))

(check-not-x860?
 '(program ()
    ((start . (block ()
                (movq (int 10) (var x)))))))


;; psuedo-x86

(check-x860*?
 '(program ()
    ((start . (block ()
                (movq (int 10) (var x)))))))

