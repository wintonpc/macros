#lang racket

(provide (all-defined-out))

(define-syntax while
  (syntax-rules ()
    [(_ test expr ...)
     (let lp ()
       (when test
         expr ...
         (lp)))]))
