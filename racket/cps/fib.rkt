#lang eopl

(define fib
  (lambda (n)
    (if (or (= n 0)
            (= n 1))
      1
      (+ (fib (- n 1))
         (fib (- n 2))))))


(define fibcps
  (lambda (n)
    (fib/k n (lambda (val)
               val))))

(define fib/k
  (lambda (n cont)
    (if (or (= n 0)
            (= n 1))
      (cont 1)
      (fib/k (- n 1) (lambda (val1)
                       (fib/k (- n 2)
                              (lambda (val2)
                                (cont (+ val1 val2)))))
             ))))

(display (fib 5))(newline)
(display (fibcps 5))(newline)
