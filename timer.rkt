#lang racket

(provide (all-defined-out))

(require data/heap)
(require "util.rkt")

(struct timer (abs-millis action) #:transparent)

(define (timer<? a b)
  (< (timer-abs-millis a) (timer-abs-millis b)))

(define (start-timer-thread)
  (let ([timers (make-heap timer<?)]
        [owner (current-thread)])
    (define (add-timer t)
      (heap-add! timers t))
    (define (secs-until t)
      (let ([ms (max 0 (- (timer-abs-millis t) (current-milliseconds)))])
        (/ ms 1000)))
    (define (wait secs)
      (if (sync/timeout secs (thread-receive-evt))
          (add-timer (thread-receive))
          (thread-send owner (heap-pop! timers)))
      (wait (and (heap-any? timers) (secs-until (heap-min timers)))))
    
    (thread (lambda () (wait #f)))))

