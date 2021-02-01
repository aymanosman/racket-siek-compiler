#lang racket

(provide program-info
         program-code)

(define program-info
  (match-lambda
    [`(program ,info ,_)
     info]))

(define program-code
  (match-lambda
    [`(program ,_ ,code)
     code]))
