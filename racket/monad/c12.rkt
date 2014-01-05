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


; 3. power-partials


; return Ma by return-writer
; write d to log at the same time
(define tell-return
  (lambda (d a)
    (bind-writer (tell-writer d)
                 (lambda (_)
                   (return-writer a)))))

; (x^n . partials)
(define power-partials
  (lambda (x n)
    (cond
      [(zero? n)
       (return-writer n)]
      [(= n 1)
       (return-writer x)]
      [(odd? n)
       (bind-writer
         (power-partials x (sub1 n))
         (lambda (d)
           (tell-return d (* x d))
           ))]
      [(even? n)
       (let ((nhalf (/ n 2)))
         (bind-writer
           (power-partials x nhalf)
           (lambda (y)
             (tell-return y (* y y)))))])))


(check equal?
       (power-partials 2 6)
       '(64 2 4 8))

(check equal?
       (power-partials 3 5)
       '(243 3 9 81))

(check equal?
       (power-partials 5 7)
       '(78125 5 25 125 15625))

;
