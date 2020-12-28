#lang racket

(provide (struct-out node)
         node-unavailable-add!
         node<=?
         node-saturation
         node-saturation=?)

(struct node (name unavailable) #:transparent)

(define (node-unavailable-add! n c)
  (set-add! (node-unavailable n) c))

(define (node<=? x y)
  (<= (- (node-saturation x)) (- (node-saturation y))))

(define (node-saturation x)
  (set-count (node-unavailable x)))

(define (node-saturation=? x y)
  (= (node-saturation x) (node-saturation y)))
