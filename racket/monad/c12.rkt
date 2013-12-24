#lang racket

(require "../base/utils.rkt"
         "utils.rkt"
         "maybe.rkt"
         "writer.rkt")

; 1. assv
(define m-assv
  (lambda (v lst)
    (cond
      [(null? lst)
       (fail)]
      [(eq? v (car (car lst)))
       (return-maybe (cdr (car lst)))]
      [else
        (m-assv v (cdr lst))])))

(check equal?
       (m-assv 'c '((a . 1) (b . 2) (c . 3)))
       '(Just 3))

(check equal?
       (m-assv 'd '((a . 1) (b . 2) (c . 3)))
       '(Nothing))

; TODO:1.1: return all in lazily

; 2. partition

(define m-partition
  (lambda (pred lst)
    (cond
      ((null? lst)
       (return-writer '()))
      ((pred (car lst))
       (bind-writer
         (tell-writer (car lst))
         (lambda (_)
           (m-partition pred (cdr lst)))))
      (else
        (bind-writer
          (m-partition pred (cdr lst))
          (lambda (d)
            (return-writer (cons (car lst)
                                 d)))))
      )))

(check equal?
       (m-partition even? '(1 2 3 4 5 6 7 8 9 10))
       '((1 3 5 7 9) . (2 4 6 8 10)))

(check equal?
  (m-partition odd? '(1 2 3 4 5 6 7 8 9 10))
  '((2 4 6 8 10) . (1 3 5 7 9)))
