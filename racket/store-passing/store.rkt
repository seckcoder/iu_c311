#lang eopl

; another impl modeling store based on scheme's own store

(require "../base/utils.rkt")

(provide (all-defined))

(define empty-store
  (lambda ()
    '()))


(define store?
  (lambda (v)
    (list? v)))

(define reference?
  (lambda (ref)
    (number? ref)))

(define-datatype
  refstore refstore?
  (rs
    (ref reference?)
    (store store?)))

(define newref
  (lambda (val store)
    (let ((ref (length store)))
      (rs ref (append store (list val))))))

(define deref
  (lambda (ref store)
    (list-ref store ref)))

(define setref
  (lambda (ref new-val store)
    (cond ((null? store)
           (eopl:error 'setref "no reference:~s in store:~s" ref store))
          ((zero? ref)
           (cons new-val
                 (cdr store)))
          (else
            (cons (car store)
                  (setref (- ref 1) new-val (cdr store)))))))

(define test-store
  (lambda ()
    (let ((store (empty-store)))
      (cases
        refstore (newref 'val store)
        (rs
          (ref store)
          (check eq? (deref ref store) 'val)
          (let ((store (setref ref 'newval store)))
            (check eq? (deref ref store) 'newval)))))))
