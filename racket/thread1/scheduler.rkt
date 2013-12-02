#lang eopl

(require "../base/utils.rkt")
(require "ds.rkt")
(require data/queue)

(provide (all-defined-out))

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

(define ready-queue-size
  (lambda ()
    (queue-length the-ready-queue)))

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
        ; tail call
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

(define wait-for-mutex
  (lambda (mtx thd)
    (if (closed-mutex? mtx)
      (begin
        (en-mutex-wait-queue! mtx thd)
        ; tail call
        (run-next-thread))
      (begin
        (close-mutex! mtx)
        (thread-start thd)))))

; this implementation is wrong. After we dequeue a mutex
; from the wait queue, we should lock the mutex immediately
; since the mutex is already locked!
#|(define signal-for-mutex
  (lambda (mtx thd)
    (println "signal for mutex:~s" (closed-mutex? mtx))
    (if (closed-mutex? mtx)
      (open-mutex! mtx)
      'ok)
    (if (mutex-wait-queue-empty? mtx)
      'ok
      (de-mutex-wait-queue! mtx
                            (lambda (front fq)
                              (place-on-ready-queue! front))))
    ; tail call
    (thread-start thd)))|#

(define signal-for-mutex
  (lambda (mtx thd)
    (if (closed-mutex? mtx)
      (if (mutex-wait-queue-empty? mtx)
        (open-mutex! mtx)
        (de-mutex-wait-queue! mtx
                              (lambda (front fq)
                                (place-on-ready-queue! front)))))
    (thread-start thd)))
