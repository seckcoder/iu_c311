#lang eopl

(require "queue.rkt")
(require "utils.rkt")
(require racket/match)

(define test
  (lambda ()
    (let ((fq (empty-fq)))
      (check eq?
             (defq1 (enfq (enfq (enfq fq 3) 4) 5))
             3)
      (check eq?
             (let ((q (enfq (enfq (enfq (enfq (empty-fq) 3) 4) 4) 5)))
               (match (fq-find (lambda (v)
                                 (= v 5))
                               q)
                 [(list finded? v idx rest ...) 
                  idx]))
             3)
      (check eq?
             (let ((q (empty-fq)))
               (match (fq-find (lambda (v)
                                 (= v 4))
                               q)
                 [(list finded? rest ...)
                  finded?]))
             #f)
      (check equal?
             (flatmap (lambda (v)
                        v)
                      '((1 2) (3 4)))
             '(1 2 3 4))
      )))
