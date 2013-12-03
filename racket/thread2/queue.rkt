; imperative queue based on store and functional queue

#lang eopl

(require "store.rkt")
(require "../base/queue.rkt")
(require racket/match)

(provide make-queue
         enqueue!
         dequeue!
         queue-filter!
         queue-empty?
         queue?
         queue-find-and-remove!
         queue-find)

(define make-queue
  (lambda ()
    (let ((ref (newref (empty-fq))))
      (define enq!
        (lambda (v)
          (let ((fq (deref ref)))
            (setref! ref (enfq fq v))
            self)))
      (define deq!
        (lambda ()
          (let ((fq (deref ref))
                (v '()))
            (defq fq (lambda (front rest)
                       (setref! ref rest)
                       (set! v front)))
            v)))
      (define find
        (lambda (handle)
          (let ((fq (deref ref)))
            (fq-find handle fq))))
      (define filter!
        (lambda (pred)
          (let ((fq (deref ref)))
            (setref! ref (fq-filter pred fq)))))
      (define empty?
        (lambda ()
          (let ((fq (deref ref)))
            (fq-empty? fq))))
      (define self
        (lambda (action)
          (cond ((eq? action 'enq) enq!)
                ((eq? action 'deq) deq!)
                ((eq? action 'find) find)
                ((eq? action 'filter) filter!)
                ((eq? action 'empty) empty?)
                (else
                  (eopl:error 'imperative-queue "unknown action")))))
      self)))

(define enqueue!
  (lambda (q v)
    ((q 'enq) v)))

(define dequeue!
  (lambda (q)
    ((q 'deq))))

(define queue-find
  (lambda (handle q)
    ((q 'find) handle)))

(define queue-filter!
  (lambda (handle q)
    ((q 'filter) handle)))

(define queue-empty?
  (lambda (q)
    ((q 'empty))))

(define queue?
  (lambda (q)
    (procedure? q)))

(define queue-find-and-remove!
  (lambda (handle q)
    (match (queue-find handle q)
      [(list finded? rest ...)
       (if finded?
         (begin
           (queue-filter! (lambda (v)
                            (not (handle v)))
                          q)
           #t)
         #f)])))
