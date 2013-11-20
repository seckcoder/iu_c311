#lang eopl

; a store implementation based on scheme's store system.
(define newref
  (lambda (v)
    (define -deref
      (lambda ()
        v))
    (define -setref
      (lambda (newv)
        (set! v newv)))
    (define m
      (lambda (action)
        (cond ((eq? action 'deref) -deref)
              ((eq? action 'setref) -setref)
              (else
                (eopl:error 'ref "unknown action:~s" action)))))
    m))

(define setref
  (lambda (ref v)
    ((ref 'setref) v)))

(define deref
  (lambda (ref)
    ((ref 'deref))))

