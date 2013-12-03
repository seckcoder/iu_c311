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
(define the-blocked-queue 'uninitialized)
(define the-mail-boxes 'uninitialized)

(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (make-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)
    (set-initial-thread-id! 0)
    (set-current-thread-id! 0) ; 0 is the thread id of the main thread
    (set-mutex-queue! (make-queue))
    (set! the-blocked-queue (make-queue))
    (set! the-mail-boxes (make-hasheq))
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
                               th-id))))
      (println "here:~s" th-id)
      (cond ((queue-find-and-remove! thid-eq-handle
                                     the-ready-queue)
             (println "find in ready queue")
             #t)
             ; find in blocked queue
            ((queue-find-and-remove! thid-eq-handle
                                    the-blocked-queue)
             (println "find in blocked queue")
             #t)
             ; find in all mutexes
            ((match (queue-find (lambda (mtx)
                                  (let* ((wait-queue-ref (mutex-wait-queue-ref mtx))
                                         (wait-queue (deref wait-queue-ref)))
                                    (match (fq-find thid-eq-handle wait-queue)
                                      [(list finded? rest ...)
                                       (println "there")
                                       (if finded?
                                         (begin
                                           (setref! wait-queue-ref
                                                    (fq-filter (lambda (v)
                                                                 (not (thid-eq-handle v)))
                                                               wait-queue))
                                           #t)
                                         #f)])))
                                (get-mutex-queue))
               [(list finded? rest ...) finded?])
             (println "find in wait queue")
             #t)
            (else
              (println "not found")
              #f)
           )
      )))

(define partial-thread-start
  (lambda (partial-thd exp-val)
    ((cadr partial-thd) exp-val)))

(define send-msg
  (lambda (to-th-id msg)
    (println "thread:~s send ~s a msg:~s" (current-thread-id) to-th-id msg)
    (match (queue-find
          (lambda (partial-thd)
            (= to-th-id (thread-id partial-thd)))
          the-blocked-queue)
      [(list finded? partial-thd rest ...)
       (if finded?
         (begin
           (queue-filter!
             (lambda (partial-thd)
               (not (= to-th-id (thread-id partial-thd))))
             the-blocked-queue)
           (place-on-ready-queue! (remake-thread
                                    (lambda ()
                                      (partial-thread-start partial-thd msg))))
           )
         (let ((mbox (hash-ref! the-mail-boxes to-th-id make-queue)))
           (enqueue! mbox msg)))])))

(define receive-msg
  (lambda (partial-thread)
    (let ((th-id (thread-id partial-thread)))
      (let ((mbox (hash-ref! the-mail-boxes th-id make-queue)))
        (if (queue-empty? mbox)
          (begin
            (enqueue! the-blocked-queue partial-thread)
            (run-next-thread))
          (partial-thread-start (dequeue! mbox)))))))
