#lang racket

(provide (all-defined-out))

(require data/heap)

(define-syntax while
  (syntax-rules ()
    [(_ test expr ...)
     (let lp ()
       (when test
         expr ...
         (lp)))]))

(define (heap-pop! h)
  (let ([v (heap-min h)])
    (heap-remove-min! h)
    v))

(define (heap-any? h)
  (> (heap-count h) 0))
