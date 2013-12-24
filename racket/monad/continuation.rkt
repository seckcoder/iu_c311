#lang racket

(require "../base/utils.rkt")

; continuation monad


; without using monad
(define rember-evens-cps
  (lambda (l k)
    (cond
      ((null? l) (k l))
      ((list? (car l))
       (rember-evens-cps (car l)
                     (lambda (a)
                       (rember-evens-cps (cdr l)
                                     (lambda (d)
                                       (k (cons a d)))))))
      ((odd? (car l))
       (rember-evens-cps (cdr l)
                     (lambda (d)
                       (k (cons (car l) d)))))
      (else
        (rember-evens-cps (cdr l)
                      (lambda (d)
                        (k d)))))))

; (rember-evens-cps '((1 2) 3 4 (5 6) 7 8) (lambda (v) v))

; using monad;
; we need to remove k from the function
(define rember-evens
  (lambda (l)
    (cond
      ((null? l) (unit '()))
      ((list? (car l))
       (bind
         (rember-evens (car l))
         (lambda (a)
           (bind
             (rember-evens (cdr l))
             (lambda (d)
               (unit (cons a d)))))))
      ((odd? (car l))
       (bind
         (rember-evens (cdr l))
         (lambda (d)
           (unit (cons (car l) d)))))
      (else
        (bind
          (rember-evens (cdr l))
          (lambda (d)
            (unit d))))
      )))

(define unit
  (lambda (a)
    ; we evaluate rember-evens in the arg of bind, so we need to keep the return
    ; of rember-evens simple
    (lambda (k)
      (k a)
      )))

(define bind
  (lambda (ma f)
    ; mb
    (lambda (k)
      (ma (lambda (a)
            ; (f a) return mb, which is a procedure
            ((f a) k))))))

#|((rember-evens '((1 2) 3 4 (5 6) 7 8)) (lambda (v)
                         v))|#


; call/cc

(define product
  (lambda (ls exit)
    (cond
      ((null? ls) (unit 1))
      ((list? (car ls))
       (bind (product (car ls) exit)
             (lambda (a)
               (bind
                 (product (cdr ls) exit)
                 (lambda (d)
                   (unit (* a d)))))))
      ((zero? (car ls))
       (bind (exit 0)
             (lambda (_)
               ; This piece of code is just for verification purpose
               (printf "not evaluted code\n")
               (unit (sub1 _)))))
      (else
        (bind (product (cdr ls) exit)
              (lambda (d)
                (unit (* (car ls) d)))))
      )))

(define callcc
  (lambda (f)
    (lambda (k)
      (let ((k-as-proc (lambda (a)
                         (lambda (k-ignored)
                           (k a)))))
        ((f k-as-proc) k)))))

((callcc (lambda (out-k)
           (product '() out-k)))
 (lambda (v)
   (printf "k\n")
   v))


((bind (callcc (lambda (out)
                 (product '() out)))
       (lambda (a)
         (unit (add1 a))))
 (lambda (x) x))
