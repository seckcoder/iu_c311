#lang eopl

(require "utils.rkt")

(provide (all-defined))

; a functional queue

(define empty-fq
  (lambda ()
    '()))

(define enfq
  (lambda (fq v)
    (append fq (list v))))

(define fq-empty?
  (lambda (fq)
    (null? fq)))

(define defq
  (lambda (fq callback)
    (if (fq-empty? fq)
      (eopl:error 'defq "empty queue")
      (callback (car fq) (cdr fq)))))
