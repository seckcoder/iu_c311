#lang eopl

(require "../base/utils.rkt")

(provide (all-defined))

(define max-time-slice-num 3)
(define the-current-thread 'uninitialized)
(define current-thread
  (lambda ()
    the-current-thread))
(define set-current-thread!
  (lambda (thd)
    (set! the-current-thread thd)))

(define make-thread
  (lambda (exp env cont initial-time result)
    (vector exp
            env
            cont
            initial-time
            result)))

(define make-new-thread
  (lambda (exp env cont)
    (make-thread exp env cont 0 'uninitialized)))

(define thread-exp
  (lambda (thd)
    (vector-ref thd 0)))
(define set-thread-exp!
  (lambda (thd new-exp)
    (vector-set! thd 0 new-exp)))

(define thread-env
  (lambda (thd)
    (vector-ref thd 1)))
(define set-thread-env!
  (lambda (thd new-env)
    (vector-set! thd 1 new-env)))

(define thread-cont
  (lambda (thd)
    (vector-ref thd 2)))
(define set-thread-cont!
  (lambda (thd new-cont)
    (vector-set! thd 2 new-cont)))

(define thread-time
  (lambda (thd)
    (vector-ref thd 3)))
(define set-thread-time!
  (lambda (thd new-time)
    (vector-set! thd 3 new-time)))
(define thread-reset-time!
  (lambda (thd)
    (set-thread-time! thd 0)))
(define thread-inc-time!
  (lambda (thd)
    (set-thread-time! thd (add1 (thread-time thd)))))
(define thread-expired?
  (lambda (thd)
    (>= (thread-time thd)
        max-time-slice-num)))

(define thread-result
  (lambda (thd)
    (vector-ref thd 4)))
(define set-thread-result!
  (lambda (thd new-result)
    (vector-set! thd 4 new-result)))

(define thread?
  (lambda (v)
    (vector? v)))
