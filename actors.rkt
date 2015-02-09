#lang racket

(provide (all-defined-out))

(require data/queue)
(require "util.rkt")
(require "timer.rkt")

;; process model

(define *processes* '())
(define *ready* (make-queue))
(define *timer-thread* (start-timer-thread))

(struct process (id mailbox (thunk #:mutable) (alive #:mutable)) #:transparent)

(define (next-id) (string->symbol (symbol->string (gensym))))
(define (make-mailbox) (make-queue))
(define (new-process) (process (next-id) (make-mailbox) #f #t))

(define (add-process p)
  (set! *processes* (cons p *processes*)))

(define (remove-process p)
  (set! *processes* (remq p *processes*)))

(define (enqueue-msg p msg)
  (enqueue! (process-mailbox p) msg))

(define (prepend-msg p msg)
  (enqueue-front! (process-mailbox p) msg))

(define (dequeue-msg p)
  (dequeue! (process-mailbox p)))

(define (find-pid pid)
  (let ([tail (memf (lambda (p) (eq? (process-id p) pid)) *processes*)])
    (and tail (car tail))))

(define (ready! p)
  (enqueue! *ready* p))

(define log printf)


;; ambient per-process variables and procedures

(define *self* (make-parameter #f))
(define *receive* (make-parameter #f))

(define-syntax (self stx)
  (syntax-case stx ()
    [_ #'(*self*)]))

(define-syntax (receive stx)
  (syntax-case stx ()
    [(_ arg ...) #'((*receive*) arg ...)]))


;; process engine

(define (start-process code . args)
  (let ([p (new-process)])
    (define (run thunk)
      (define (thunkify cont)
        (lambda () (run (lambda () (cont #f)))))
      (let ([cont (reenter thunk)])
        (set-process-thunk! p (thunkify cont))))
    (define (make-receive yield)
      (let ([timer-cancelled? #f])
        (define (receive)
          (call/cc yield)
          (set! timer-cancelled? #t)
          (dequeue-msg p))
        (case-lambda
          [() (receive)]
          [(timeout-ms timeout-msg)
           (install-timer timeout-ms
                          (lambda () (unless timer-cancelled?
                                  (send-front p timeout-msg))))
           (receive)])))
    (define (install-timer timeout-ms action)
      (let ([timer (timer (+ (current-milliseconds) timeout-ms) action)])
        (thread-send *timer-thread* timer)))
    (define (reenter thunk)
      (call/cc
          (lambda (yield)
            (parameterize ([*self* p]
                           [*receive* (make-receive yield)])
              (with-handlers ([exn:fail? on-failure])
                (thunk)
                (clean-up "exited normally"))))))
    (define (clean-up why)
      (printf "process ~a ~a~n" (process-id p) why)
      (set-process-alive! p #f)
      (remove-process p))
    (define (on-failure failure)
      (clean-up "terminated due to exception")
      (raise failure))

    (add-process p)
    (set-process-thunk! p (lambda () (run (lambda () (apply code args)))))
    (ready! p)
    (pump)
    p))

(define (continue-process p)
  (if (process-alive p)
      ((process-thunk p))
      (error 'terminated)))

(define (send p msg)
  (send-impl p msg enqueue-msg))

(define (send-front p msg)
  (send-impl p msg prepend-msg))

(define (send-impl p msg enqueue)
  (when (process-alive p)
    (enqueue p msg)
    (ready! p)
    (pump)))



(define pumping? (make-parameter #f))
(define (pump)
  (unless (pumping?)
    (parameterize ([pumping? #t])
      (while (non-empty-queue? *ready*)
        (continue-process (dequeue! *ready*))))))


;; example processes

(define (echo)
  (define (go)
    (let ([msg (receive)])
      (cond
       [(eq? msg 'exit) #f]
       [(eq? msg 'crash) (error "boom")]
       [else
        (begin
          (printf "echo: ~a~n" msg)
          (go))])))
  (printf "echo process started~n")
  (go))


(define (pinger pong)
  (send pong self)
  (let lp ([n 1])
    (when (< n 10)
      (printf "ping: ~a~n" n)
      (send pong n)
      (lp (+ (receive) 1)))))

(define (ponger)
  (let ([ping (receive)])
    (let lp ()
      (let ([n (+ (receive) 1)])
        (printf "pong: ~a~n" n)
        (send ping n)
        (lp)))))
    
(define (poller)
  (let ([msg (receive 1000 'timeout)])
    (printf "poller received: ~a~n" msg)
    (unless (eq? msg 'exit)
      (printf "poll~n")
      (poller))))
      
