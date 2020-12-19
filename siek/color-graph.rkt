#lang racket

(provide color-graph)

(require graph
         data/heap)

(define (color-graph conflict-graph move-graph)
  (define colors (make-hash))
  (define q (make-heap node<=?))
  (define nodes
    (for/hash ([v (get-vertices conflict-graph)]
               #:when (not (member v registers)))
      (define n (node v (mutable-set)))
      (heap-add! q n)
      (values v n)))
  (let loop ()
    (cond
      [(= 0 (heap-count q))
       (void)]
      [else
       (define n (heap-pop! q))
       (define conflicts (list->set (get-neighbors conflict-graph (node-name n))))
       (define move-related (and move-graph (list->set (get-neighbors move-graph (node-name n)))))
       (define c (choose-color colors conflicts move-related))
       (hash-set! colors (node-name n) c)
       (for ([n (stream-map (lambda (v) (hash-ref nodes v #f)) conflicts)]
             #:when n)
         (node-sat-add! n c)
         (heap-notify! q n))
       (loop)]))
  colors)

(define registers '(rax))

(define (choose-color colors conflicts move-related)
  (or (move-color colors conflicts move-related)
      (available-color colors conflicts)))

(define (move-color colors conflicts move-related)
  (for/first ([m move-related]
              #:when (and (not (set-member? conflicts m))
                          ;; TODO reject stack locations
                          (hash-has-key? colors m)))
    (hash-ref colors m #f)))

(define (available-color colors conflicts)
  (local-require threading)
  (define assigns
    (~> conflicts
        (stream-map (lambda (v) (hash-ref colors v #f)) _)
        (stream-filter identity _)
        stream->list
        (sort _ <=)))

  (for/fold ([c 0])
            ([d assigns]
             #:break
             (not (= c d)))
    (add1 c)))

;; Aux

(struct node (name sat) #:transparent)

(define (node-sat-add! n c)
  (set-add! (node-sat n) c))

(define (node=? x y)
  (= (- (node-saturation x)) (- (node-saturation y))))

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
