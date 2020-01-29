#lang racket

(require rackunit)

(require "check-x860.rkt")

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

