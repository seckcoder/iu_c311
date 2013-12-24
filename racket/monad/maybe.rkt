#lang racket
(require "utils.rkt")

(provide return-maybe
         bind-maybe
         fail)

; Maybe monad demo
; Understanding Maybe Monad:
; 1. Why do we need monad?
;    providing better api. abstraction.
; 2. What's monad?
;    return, bind

; a -> Ma
; move a to monadic world
(define return-maybe
  (lambda (a) `(Just ,a)))

; Ma * (a -> Mb) -> Mb
; do some transformation(bring some effect).
(define bind-maybe
  (lambda (ma f)
    (cond
      [(eq? (car ma) 'Just) (f (cadr ma))]
      [(eq? (car ma) 'Nothing) (fail)])))

(define fail
  (lambda ()
    '(Nothing)))

(define (divide-maybe x y)
  (if (= y 0)
    (fail)
    (return-maybe (/ x y))))

#|(bind-maybe
  (return-maybe 2)
  (lambda (x)
    (bind-maybe
      (divide-maybe x 0)
      (lambda (x)
        (divide-maybe x 5)))))|#


; without using monad
(define divide-multi1
  (lambda (a b c)
    (let ((r1 (divide-maybe a b)))
      (cond
        ((eq? (car r1) 'Nothing)
         '(Nothing))
        (else
          (let ((r2 (divide-maybe (car r1) c)))
            (cond
              ((eq? (car r2) 'Nothing)
               '(Nothing))
              (else
                `(Just, (car r2))))))))))

; (divide-multi1 2 0 3)

; using monad
(define divide-multi2
  (lambda (a b c)
    (bind-maybe
      (divide-maybe a b)
      (lambda (x)
        (bind-maybe
          (divide-maybe x c)
          return-maybe)))))

; (divide-multi2 2 1 3)

; using do
(define divide-multi3
  (lambda (a b c)
    (mdo
      bind-maybe
      (x <- (divide-maybe a b))
      (x <- (divide-maybe x c))
      (return-maybe x))))

; (divide-multi3 2 1 3)
