#lang racket
(require "utils.rkt")

; writer monad

(define reciprocals1
  (lambda (l)
    (cond
      ((null? l) (unit-writer '()))
      ((= (car l) 0)
       (bind-writer
         (tell-writer "saw a 0")
         (lambda (_)
           (reciprocals1 (cdr l)))))
      (else
        (bind-writer
          (reciprocals1 (cdr l))
          (lambda (d)
            (unit-writer
              (cons (/ 1 (car l))
                    d))))))))

(define bind-writer
  (lambda (ma f)
    (let ((mb (f (car ma))))
      `(,(car mb) ,(append (cdr ma) (cdr mb))))))

(define tell-writer
  (lambda (msg)
    `(_ . (,msg))))

(define unit-writer
  (lambda (a)
    `(,a . ())))

; (reciprocals1 '(1 2 0 3 0 4))

(define reciprocals2
  (lambda (l)
    (cond
      ((null? l) (unit-writer '()))
      ((zero? (car l))
       (mdo
         bind-writer
         (_ <- (tell-writer "saw a 0"))
         (reciprocals2 (cdr l))))
      (else
        (mdo
          bind-writer
          (d <- (reciprocals2 (cdr l)))
          (unit-writer (cons (/ 1 (car l))
                             d)))))))

; (reciprocals2 '(1 2 0 3 0 4))

