#lang racket

(require "../base/utils.rkt")

; continuation monad


; without using monad
(define rember-evens-cps
  (lambda (l k)
    (cond
      ((null? l) (k l))
      ((list? (car l))
       (rember-evens-cps (car l)
                     (lambda (a)
                       (rember-evens-cps (cdr l)
                                     (lambda (d)
                                       (k (cons a d)))))))
      ((odd? (car l))
       (rember-evens-cps (cdr l)
                     (lambda (d)
                       (k (cons (car l) d)))))
      (else
        (rember-evens-cps (cdr l)
                      (lambda (d)
                        (k d)))))))

; (rember-evens-cps '((1 2) 3 4 (5 6) 7 8) (lambda (v) v))

; using monad;
; we need to remove k from the function
(define rember-evens
  (lambda (l)
    (cond
      ((null? l) (unit '()))
      ((list? (car l))
       (bind
         (rember-evens (car l))
         (lambda (a)
           (bind
             (rember-evens (cdr l))
             (lambda (d)
               (unit (cons a d)))))))
      ((odd? (car l))
       (bind
         (rember-evens (cdr l))
         (lambda (d)
           (unit (cons (car l) d)))))
      (else
        (bind
          (rember-evens (cdr l))
          (lambda (d)
            (unit d))))
      )))

(define unit
  (lambda (a)
    ; we evaluate rember-evens in the arg of bind, so we need to keep the return
    ; of rember-evens simple
    (lambda (k)
      (k a)
      )))

(define bind
  (lambda (ma f)
    ; mb
    (lambda (k)
      (ma (lambda (a)
            ; (f a) return mb, which is a procedure
            ((f a) k))))))

((rember-evens '((1 2) 3 4 (5 6) 7 8)) (lambda (v)
                         v))

;(((rember-evens '((1 2) 3 4 (5 6) 7 8)) (lambda (v) v)) (lambda (v) v))
