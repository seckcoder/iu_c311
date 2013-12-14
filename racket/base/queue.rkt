; a functional queue

#lang racket

(require "utils.rkt")

(require racket/match)
(provide (all-defined-out))


(define empty-fq
  (lambda ()
    '()))

(define enfq
  (lambda (fq v)
    (append fq (list v))))

(define fq?
  (lambda (q)
    (list? q)))

(define fq-empty?
  (lambda (fq)
    (null? fq)))

(define defq
  (lambda (fq callback)
    (if (fq-empty? fq)
      (error 'defq "empty queue")
      (callback (car fq) (cdr fq)))))

(define defq1
  (lambda (fq)
    (if (fq-empty? fq)
      (error 'defq "empty queue")
      (car fq))))

(define fq-find
  (lambda (handle fq)
    (find handle fq)))
  
(define fq-filter
  (lambda (pred fq)
    (filter pred fq)))
