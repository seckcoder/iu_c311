#lang eopl

(require "queue.rkt")
(require "utils.rkt")

(define test
  (lambda ()
    (let ((fq (empty-fq)))
      (check eq?
             (defq1 (enfq (enfq (enfq fq 3) 4) 5))
             3))))
