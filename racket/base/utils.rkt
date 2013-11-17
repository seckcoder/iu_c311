#lang eopl

(provide (all-defined))

(define anything?
  (lambda (v)
    #t))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))


(define index-of
  (lambda (lst v)
    (define (iter lst v idx)
      (cond ((null? lst) -1)
            ((eq? (car lst) v) idx)
            (else
              (iter (cdr lst) v (+ 1 idx)))))
    (iter lst v 0)))
