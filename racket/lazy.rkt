#lang racket

(require racket/promise)

(provide (all-defined-out))

;; There is a problem with the library.
;; For the following demo:
;; (s:map (lambda (s)
;;           s)
;;        (s:take n lazy-stream))
;; if lazy-stream is infinite, then it will cause
;; infinite loop. The problem here is that map
;; 's arguments are not stricly lazy. They will be
;; evaluated.
          
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


(define (s:take n l)
  (if (or (= n 0)
          (s:null? l))
    '()
    (s:cons (s:car l)
            (s:take (- n 1)
                    (s:cdr l)))))


(define (s:append1 s1 s2)
  (if (s:null? s1)
    (force s2)
    (s:cons (s:car s1)
            (s:append1 (s:cdr s1)
                       s2))))

; this version works even when s1 is infinite stream
(define-syntax s:append
  (syntax-rules ()
    [(_ s1 s2)
     (s:append1 s1 (delay s2))]))

(define (s:flatmap p l)
  (if (s:null? l)
    '()
    (s:append (p (s:car l))
              (s:flatmap p (s:cdr l)))))

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
    (check-equal? (s:list-ref (s:flatmap (lambda (v) (list v)) s:ones)
                              10) 1)
    ))
