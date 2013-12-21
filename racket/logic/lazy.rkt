#lang lazy

(provide (all-defined-out))

(define (foo)
  (display "hello world")(newline))

(define (flatmap p l)
  (if (null? l)
    '()
    (append (p (car l))
            (flatmap p (cdr l)))))

(define (safe-take n l)
  (if (or (= n 0)
          (null? l))
    '()
    (cons (car l)
          (safe-take (- n 1)
                     (cdr l)))))


(define ones (cons 1 ones))

(module+ test
  (list-ref
       (flatmap (lambda (v)
                  (list v))
                ones)
       10)
  )
