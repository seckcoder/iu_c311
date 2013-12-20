#lang racket

(require racket/promise)

(provide (all-defined-out))

(define-syntax s:cons
  (syntax-rules ()
    [(_ a d)
     (cons a (delay d))]))

(define s:car car)
(define s:null? null?)
(define s:list? list?)
(define s:pair? pair?)
(define (s:cdr l)
  (force (cdr l)))

(define-syntax s:list
  (syntax-rules ()
    [(_) '()]
    [(_ v^ v ...)
     (s:cons v^
             (s:list v ...))]))

(define (s:map p . args)
  (if (s:null? (car args))
    '()
    (s:cons (apply p (map s:car args))
            (apply s:map (cons p (map s:cdr args))))))

(define (s:filter p l)
  (if (s:null? l)
    '()
    (let ((v (s:car l)))
      (if (p v)
        (s:cons v (s:filter p (s:cdr l)))
        (s:filter p (s:cdr l))))))

(define (s:foldl p acc . args)
  (let loop ((acc acc)
             (args args))
    (if (s:null? (car args))
      acc
      (loop (apply p
                   (append (map s:car args)
                           (list acc)))
            (map s:cdr args)))))

(define (s:take n l)
  (if (or (= n 0)
          (s:null? l))
    '()
    (s:cons (s:car l)
            (s:take (- n 1)
                    (s:cdr l)))))

(define (s:append s1 s2)
  (if (s:null? s1)
    s2
    (s:cons (s:car s1)
            (s:append (s:cdr s1)
                      s2))))

(define (s:flatmap p l)
  (s:foldl (lambda (v acc)
             (s:append acc (p v)))
           '()
           l))

(define (s:list-ref l n)
  (let loop ((l l)
             (i 0))
    (if (= i n)
      (s:car l)
      (loop (s:cdr l)
            (+ i 1)))))

(define (s:interleave s1 s2)
  (if (s:null? s1)
    s2
    (s:cons (s:car s1)
            (s:interleave s2
                          (s:cdr s1)))))

(define (s:! v)
  (if (promise? v)
    (force v)
    v))

(define (s:!! v)
  (if (s:pair? v)
    (cons (s:!! (s:car v))
          (s:!! (s:cdr v)))
    (s:! v)))

(define s:ones (s:cons 1 s:ones))
(define (s:geo-series base ratio)
  (s:cons base
          (s:geo-series (* base ratio)
                        ratio)))
(module+ test
  (require rackunit)
  (let* ((lst (s:list 1 2 3 4))
         (lst1 (s:list lst
                      (s:list 5 6 7 8))))
    (check-eq? (s:car lst) 1)
    (check-eq? (s:car (s:cdr lst)) 2)
    (check-equal? (s:!! (s:map (lambda (v) v) lst))
                  '(1 2 3 4))
    (check-equal? (s:!! lst1)
                  '((1 2 3 4)
                    (5 6 7 8)))
    (check-equal? (s:!! (s:flatmap (lambda (v) v) lst1))
                  '(1 2 3 4 5 6 7 8))
    (check-eq? (s:list-ref s:ones 10) 1)
    (check-eq? (s:list-ref (s:geo-series 1 2)
                           10)
               1024)
    (check-equal? (s:!! (s:filter (lambda (v)
                                    (= (modulo v 2) 0))
                                  lst))
                        '(2 4))
    ))
