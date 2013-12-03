#lang eopl

(define fact
  (lambda (n)
    (fact/k n 1)))

(define fact/k
  (lambda (n k)
    (if (zero? n)
      k
      (begin
        (display k)(display " ")(display n)(newline)
        (fact/k (- n 1) (* k n))))))


(define fact1
  (lambda (n)
    (fact/k1 n (lambda (var) var))))

(define fact/k1
  (lambda (n k)
    (if (zero? n)
      (k 1)
      (fact/k1 (- n 1) (lambda (val)
                        (display val)(display " ")(display n)(newline)
                        (k (* val n)))))))
(fact 5)
(newline)
(fact1 5)
