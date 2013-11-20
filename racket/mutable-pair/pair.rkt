#lang eopl

(provide (all-defined))

(require "store1.rkt")

(define mutpair?
  (lambda (v)
    (reference? v)))

(define make-pair
  (lambda (left right)
    (car (newrefs (list left right)))))

(define pleft
  (lambda (pair)
    (deref pair)))

(define pright
  (lambda (pair)
    (deref (+ 1 pair))))

(define setleft
  (lambda (pair left)
    (setref! pair left)))

(define setright
  (lambda (pair right)
    (setref! (+ 1 pair) right)))
