#lang racket

(provide color-graph)

(require graph
         data/heap)

(define (color-graph conflict)
  (define colors (make-hash))
  (define q (make-heap node<=?))
  (define nodes
    (for/hash ([v (get-vertices conflict)])
      (define n (node v (mutable-set)))
      (heap-add! q n)
      (values v n)))
  (let loop ()
    (cond
      [(= 0 (heap-count q))
       (void)]
      [else
       (define n (heap-pop! q))
       (define adj (get-neighbors conflict (node-name n)))
       (define c (find-lowest-color colors adj))
       (hash-set! colors (node-name n) c)
       (for ([n (stream-map (lambda (v)
                              (hash-ref nodes v))
                            adj)])
         (node-sat-add! n c)
         (heap-notify! q n))
       (loop)]))
  colors)

(define (find-lowest-color colors adj)
  (define assigns
    (sort
     (stream->list
      (stream-filter
       identity
       (stream-map
        (lambda (v) (hash-ref colors v #f))
        adj)))
     <=))
  (for/fold ([c 0])
            ([d assigns]
             #:break
             (not (= c d)))
    (add1 c)))

;; Aux

(struct node (name sat) #:transparent)

(define (node-sat-add! n c)
  (set-add! (node-sat n) c))

(define (node<=? x y)
  (<= (- (node-saturation x)) (- (node-saturation y))))

(define (node-saturation x)
  (set-count (node-sat x)))

(define (heap-pop! q)
  (begin0 (heap-min q)
          (heap-remove-min! q)))

(define (heap-notify! q n)
  (and (heap-remove! q n)
       (heap-add! q n)))
