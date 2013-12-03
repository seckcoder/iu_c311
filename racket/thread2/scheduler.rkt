#lang eopl

(require "../base/utils.rkt")
(require "../base/queue.rkt")
(require "ds.rkt")
(require "queue.rkt")
(require "store.rkt")
(require racket/match)

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
    (set! the-time-remaining the-max-time-slice)
    (set-initial-thread-id! 0)
    (set-current-thread-id! 0) ; 0 is the thread id of the main thread
    (set-mutexes! '())
    ))

(define place-on-ready-queue!
  (lambda (thd)
    ;(println "place thread:~s to ready queue" (thread-id thd))
    (enqueue! the-ready-queue thd)))

(define run-next-thread
  (lambda ()
    ;(println "run-next-thread")
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
    ;(println "wait for mutex")
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
    ; (println "signal for mutex:~s" thd)
    (if (closed-mutex? mtx)
      (if (mutex-wait-queue-empty? mtx)
        (open-mutex! mtx)
        (de-mutex-wait-queue! mtx
                              (lambda (front fq)
                                (place-on-ready-queue! front))))
      'ok)
    (thread-start thd)))

(define kill-thread
  (lambda (th-id)
    (let ((thid-eq-handle (lambda (thd)
                            (= (thread-id thd)
                               th-id)))
          (thid-not-eq-handle (lambda (thd)
                                (not (= (thread-id thd)
                                        th-id)))))
      (match (queue-find thid-eq-handle the-ready-queue)
        [(list finded? thd rest ...)
         (println "finded in reaady queue:~s" finded?)
         (if finded?
           (begin
             (queue-filter! thid-not-eq-handle the-ready-queue)
             #t)
           (match (find (lambda (mtx)
                          (let* ((wait-queue-ref (mutex-wait-queue-ref mtx))
                                (wait-queue (deref wait-queue-ref)))
                            (match (fq-find thid-eq-handle wait-queue)
                              [(list finded? rest ...)
                               (if finded?
                                 (begin
                                   (setref! wait-queue-ref
                                            (fq-filter thid-not-eq-handle
                                                       wait-queue))
                                   #t)
                                 #f)])))
                        (get-mutexes))
             [(list finded? rest ...)
              (println "finded in mutex queue:~s" finded?)
              finded?]))]))))
