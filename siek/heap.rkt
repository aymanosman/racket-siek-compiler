#lang racket

(provide (all-from-out data/heap)
         heap-empty?
         heap-pop!
         heap-notify!)

(require data/heap)

(define (heap-pop! q)
  (begin0 (heap-min q)
    (heap-remove-min! q)))

(define (heap-notify! q n)
  (and (heap-remove-eq! q n)
       (heap-add! q n)))

(define (heap-empty? h)
  (= 0 (heap-count h)))
