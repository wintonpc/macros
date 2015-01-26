(require data/queue)

(define *processes* '())
(define *waiting* (make-queue))
(define *ready* (make-queue))

(struct process (id mailbox (thunk #:mutable) (alive #:mutable)) #:transparent)

(define (add-process p)
  (set! *processes* (cons p *processes*)))

(define (remove-process p)
  (set! *processes* (remq p *processes*)))

(define (dequeue-msg p)
  (dequeue! (process-mailbox p)))

(define (enqueue-msg p msg)
  (enqueue! (process-mailbox p) msg))

(define (next-id) (gensym))
(define (make-mailbox) (make-queue))

(define (start-process code)
  (let ([p (process (next-id) (make-mailbox) #f #t)]
        [yield #f])
    (define (receive)
      (call/cc yield)
      (dequeue-msg p))
    (define (call-with-updated-yield thunk)
      (call/cc
          (lambda (k)
            (set! yield k)
            (thunk)
            (printf "process ~a exited~n" (process-id p))
            (set-process-alive! p #f)
            (remove-process p))))
    (define (run thunk)
      (let ([cont (call-with-updated-yield thunk)])
        (set-process-thunk! p (lambda () (run (lambda () (cont #f)))))))

    (add-process p)
    (run (lambda () (code receive)))
    p))

(define (foo receive)
  (printf "going to receive~n")
  (let ([msg (receive)])
    (printf "received ~a~n" msg)
    (printf "exiting~n")))

(define (bar receive)
  (define (go)
    (let ([msg (receive)])
      (unless (string=? msg "exit")
        (printf "bar received ~a~n" msg)
        (go))))
  (printf "starting receive loop~n")
  (go))

(define (find-pid pid)
  (memf (lambda (p) (eq? (process-id p) pid)) *processes*))

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

;(define (send pid msg)
;  (let ([p (find-pid pid)])
;    (
