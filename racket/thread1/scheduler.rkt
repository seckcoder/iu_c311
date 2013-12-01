#lang eopl

(require "../base/utils.rkt")
(require data/queue)

(provide (all-defined))

(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)

(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (make-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))

(define place-on-ready-queue!
  (lambda (thd)
    (enqueue! the-ready-queue thd)))

(define thread-start
  (lambda (thd)
    (thd)))

(define run-next-thread
  (lambda ()
    (if (queue-empty? the-ready-queue)
      the-final-answer
      (let ((thd (dequeue! the-ready-queue)))
        (set! the-time-remaining the-max-time-slice)
        (thread-start thd)))))

(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))
(define final-answer
  (lambda ()
    the-final-answer))

(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (sub1 the-time-remaining))))
