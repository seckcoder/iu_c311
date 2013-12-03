#lang eopl
(require "../base/utils.rkt")

; list-sum

(define list-sum
  (lambda (lst)
    (cond ((null? lst) 0)
          (else
            (+ (car lst)
               (list-sum (cdr lst)))))))

; cps transformation

(define list-sum1
  (lambda (lst)
    (list-sum1/k lst (lambda (val) val))))

(define list-sum1/k
  (lambda (lst cont)
    (if (null? lst)
      (cont 0)
      (list-sum1/k (cdr lst) (lambda (val)
                               (cont (+ val (car lst))))))))

; further optmize

(define list-sum2
  (lambda (lst)
    (list-sum2/k lst 0)))

(define list-sum2/k
  (lambda (lst k)
    (if (null? lst)
      k
      (list-sum2/k (cdr lst) (+ (car lst) k)))))

(check eq?
       (list-sum '(1 2 3 4))
       10)
(check eq?
       (list-sum1 '(1 2 3 4))
       10)
(check eq?
       (list-sum2 '(1 2 3 4))
       10)
