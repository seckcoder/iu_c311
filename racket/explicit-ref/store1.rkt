#lang eopl

; another impl modeling store based on scheme's own store

(require "../base/utils.rkt")

(provide (all-defined))

(define empty-store
  (lambda ()
    '()))

(define the-store 'uninitialized)
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define get-store
  (lambda ()
    the-store))

(define reference?
  (lambda (ref)
    (number? ref)))

(define newref
  (lambda (val)
    (let ((ref (length the-store)))
      (set! the-store (append the-store (list val)))
      ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define setref!
  (lambda (ref new-val)
    (set! the-store (letrec
                      ((setref-inner
                         (lambda (store1 ref1)
                           (cond ((null? store1)
                                  (eopl:error 'setref! "no reference:~s in store:~s" ref1 store1))
                                 ((zero? ref1)
                                  (cons new-val
                                        (cdr store1)))
                                 (else
                                   (cons (car store1)
                                         (setref-inner (cdr store1) (- ref1 1))))
                                 ))))
                      (setref-inner the-store ref)))))

(define test-store
  (lambda ()
    (initialize-store!)
    (let ((ref (newref 3)))
      (check eq? (deref ref) 3)
      (setref! ref 4)
      (check eq? (deref ref) 4)
      )))
