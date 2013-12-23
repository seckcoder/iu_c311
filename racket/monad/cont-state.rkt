#lang racket

(require "../base/utils.rkt")

; A mix of continuation and state monad
; I invent the monad by myself. I haven't verify it by the monad law.
; TODO: verify the monad by the monad law.

; we need to remove k and s from the function
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
            (lambda (s k)
              (k `(,d . ,(+ 1 s)))))))
      )))

(define unit
  (lambda (a)
    ; ma
    (lambda (s k)
      (k `(,a . ,s))
      )))

(define bind
  (lambda (ma f)
    ; mb
    (lambda (s k)
      (ma s (lambda (va)
              (match va
                [(cons a new-s)
                 ((f a) new-s k)]))))))

((rember-evens '((1 2) 3 4 (5 6) 7 8)) 0 (lambda (v)
                         v))

;(((rember-evens '((1 2) 3 4 (5 6) 7 8)) (lambda (v) v)) (lambda (v) v))
