#lang racket

(provide with-input)

(define-syntax-rule (with-input input body ...)
  (parameterize ([current-input-port (open-input-string input)])
    body ...))
