(require data/queue)

;; process model

(define *processes* '())
(define *waiting* (make-queue))
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

;; process engine

(define (start-process code)
  (let ([p (new-process)]
        [yield #f])
    (define (receive)
      (call/cc yield)
      (dequeue-msg p))
    (define (call-with-updated-yield thunk)
      (call/cc
          (lambda (k)
            (define (clean-up why)
              (printf "process ~a ~a~n" (process-id p) why)
              (set-process-alive! p #f)
              (remove-process p))
            (set! yield k)
            (with-handlers ([exn:fail? (lambda (raised) (clean-up "terminated due to exception") (raise raised))])
              (thunk)
              (clean-up "terminated")))))
    (define (run thunk)
      (let ([cont (call-with-updated-yield thunk)])
        (set-process-thunk! p (lambda () (run (lambda () (cont #f)))))))

    (add-process p)
    (run (lambda () (code p receive)))
    p))

(define (foo self receive)
  (printf "foo: going to receive~n")
  (let ([msg (receive)])
    (printf "foo: received ~a~n" msg)
    (printf "foo: exiting~n")))

raise(define (crasher self receive)
  (printf "crasher: going to receive~n")
  (let ([msg (receive)])
    (printf "crasher: received ~a~n" msg)
    (error "oops")))

(define (bar self receive)
  (define (go)
    (let ([msg (receive)])
      (if (string=? msg "exit")
          (error "we're done here")
          (begin
            (printf "bar received ~a~n" msg)
            (go)))))
  (printf "starting receive loop~n")
  (go))

(define (send p msg)
  (when (process-alive p)
    (enqueue-msg p msg)
    (enqueue! *ready* p)
    (run-ready)))

(define (run-ready)
  (unless (queue-empty? *ready*)
    (let ([p (dequeue! *ready*)])
      ((process-thunk p)))
    (run-ready)))


(define (ping self receive)
  (let ([pong (receive)])
    (let lp ([n (receive)])
      (printf "ping ~a~n" n))))

(define (pong) #f)

;(define (send pid msg)
;  (let ([p (find-pid pid)])
;    (
