#lang racket

(provide (all-defined-out))

(require data/queue)
(require "util.rkt")

;; process model

(define *processes* '())
(define *ready* (make-queue))

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
    [(_) #'((*receive*))]))


;; process engine

(define (start-process code . args)
  (let ([p (new-process)])
    (define (run thunk)
      (define (thunkify cont)
        (lambda () (run (lambda () (cont #f)))))
      (let ([cont (reenter thunk)])
        (set-process-thunk! p (thunkify cont))))
    (define (reenter thunk)
      (call/cc
          (lambda (yield)
            (define (receive)
              (call/cc yield)
              (dequeue-msg p))
            (parameterize ([*self* p]
                           [*receive* receive])
              (with-handlers ([exn:fail? on-failure])
                (thunk)
                (clean-up "terminated"))))))
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
  (when (process-alive p)
    (enqueue-msg p msg)
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
      (printf "ping ~a~n" n)
      (send pong n)
      (lp (+ (receive) 1)))))

(define (ponger)
  (let ([ping (receive)])
    (let lp ()
      (let ([n (+ (receive) 1)])
        (printf "pong: ~a~n" n)
        (send ping n)
        (lp)))))
    
