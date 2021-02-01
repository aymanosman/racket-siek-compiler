#lang racket

(provide color-graph)

(require graph
         "x86.rkt"
         "color-node.rkt"
         "colors-to-homes.rkt"
         "heap.rkt")

(define-logger siek #:parent (current-logger))

(define (color-graph conflict-graph
                     locals
                     [move-graph #f]
                     #:order-for-test [order-for-test #f])
  (define q (make-heap node<=?))

  (define nodes
    (for/hash ([x (remove-duplicates (append locals (get-vertices conflict-graph)))])
      (define n (node x (mutable-set)))
      (heap-add! q n)
      (values x n)))

  (define assigned (make-hash))

  (define (update-neighbours x0 c)
    (for ([x (in-neighbors conflict-graph x0)])
      (define n (hash-ref nodes x))
      (node-unavailable-add! n c)
      (heap-notify! q n)))

  (define (assign-color x c)
    (when (hash-has-key? assigned x)
      (raise-arguments-error 'color-graph "color already assigned to this variable"
                             "variable" x))
    (hash-set! assigned x c)
    (update-neighbours x c))

  (define (init-assigned)
    (for ([(reg color) (sequence-append (in-hash #hash((rax . -1)
                                                       (rsp . -2)))
                                        (in-hash register=>color))])
      (when (not (has-vertex? conflict-graph reg))
        (add-vertex! conflict-graph reg))
      (assign-color reg color)
      (when (hash-has-key? nodes reg)
        (heap-remove-eq! q (hash-ref nodes reg)))))

  (define (available-color n)
    (for/first ([c (in-naturals)]
                #:when
                (not (set-member? (node-unavailable n) c)))
      c))

  (define (nodes-with-highest-and-equal-saturation)
    (let loop ([acc (list (heap-pop! q))])
      (cond
        [(heap-empty? q)
         (reverse acc)]
        [else
         (define n (heap-pop! q))
         (if (node-saturation=? n (first acc))
             (loop (cons n acc))
             (begin
               (heap-add! q n)
               (reverse acc)))])))

  (define (move-color n)
    (cond
      [(register? (node-name n))
       #f]
      [else
       (for/first ([m (in-neighbors move-graph (node-name n))]
                   #:when
                   ;; TODO reject stack locations
                   (and (hash-has-key? assigned m)
                        (not (set-member? (node-unavailable n) (hash-ref assigned m)))))
         (hash-ref assigned m))]))

  (define (pick-candidate+color)
    (define candidates (nodes-with-highest-and-equal-saturation))

    (define move-related-colors
      (and move-graph
           (for/list ([n candidates]) (move-color n))))

    (define (restore-unpicked picked)
      (for ([n candidates] #:when (not (eq? n picked)))
        (heap-add! q n)))

    (match (for/first ([n candidates]
                       [c (or move-related-colors empty)]
                       #:when c)
             (cons n c))
      [(cons picked-node picked-color)
       (restore-unpicked picked-node)
       (values picked-node picked-color)]
      ;; default to first node
      [_
       (define picked-node (first candidates))
       (restore-unpicked picked-node)
       (values picked-node #f)]))

  (init-assigned)
  (let loop ()
    (cond
      [(heap-empty? q)
       (void)]
      [else
       (define-values (n maybe-color)
         (cond
           [order-for-test
            (define n (hash-ref nodes (first order-for-test)))
            (heap-remove-eq! q n)
            (begin0 (values n #f)
              (set! order-for-test (rest order-for-test)))]
           [else
            (pick-candidate+color)]))
       (define c (or maybe-color (available-color n)))
       (log-siek-debug "(color-graph) assigned color ~v(~v) = ~v" (node-name n) maybe-color c)
       (assign-color (node-name n) c)
       (loop)]))
  assigned)
