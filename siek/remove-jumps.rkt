#lang racket

(provide remove-jumps-R1
         remove-jumps-R2)

(require graph)

(define (remove-jumps-R1 p)
  (send (new remove-jumps-R1%) remove-jumps p))

(define (remove-jumps-R2 p)
  (send (new remove-jumps-R2%) remove-jumps p))

(define remove-jumps-R1%
  (class object%
    (super-new)

    (define/public (who)
      'remove-jumps-R1)

    (define/public (remove-jumps p)
      (local-require threading)
      (match p
        [`(program ,info ,code)
         (define control-flow (C-code->graph code))
         (define g (transpose control-flow))

         (define new-code
           (for/fold ([code code])
                     ([l (tsort g)]
                      [_n (in-range (sub1 (length code)))])
             (define n (get-neighbors g l))
             (cond
               [(and (= (length n) 1)
                     (= 1 (length (get-neighbors control-flow (first n)))))
                (define other (first n))
                (~> (dict-remove code l)
                    (dict-set _ other (merge-tail (dict-ref code other)
                                                  (dict-ref code l))))]
               [else
                code])))
         `(program ,info ,new-code)]))

    ;; see instr*->edges
    (define/public (tail->labels t)
      (match t
        [`(seq ,_ ,t)
         (tail->labels t)]
        [`(goto ,l)
         (list l)]
        [`(return ,_)
         (list)]))

    (define (merge-tail t0 t1)
      (match t0
        [`(return ,_)
         (raise-arguments-error (who) "cannot handle tail (return ...)")]
        [`(if ,_ ,_ ,_)
          (raise-arguments-error (who) "cannot handle tail (if ...)")]
        [`(goto ,_)
         t1]
        [`(seq ,s ,t)
         `(seq ,s ,(merge-tail t t1))]))

    (define (C-code->graph code)
      (directed-graph (append*
                       (for/list ([l+t code])
                         (match-define (cons label tail) l+t)
                         (map (lambda (l) (list label l)) (tail->labels tail))))))))

(define remove-jumps-R2%
  (class remove-jumps-R1%
    (super-new)

    (define/override (who)
      'remove-jumps-R2)

    (define/override (tail->labels t)
      (match t
        [`(if ,_ (goto ,l0) (goto ,l1))
         (list l0 l1)]
        [_ (super tail->labels t)]))))
