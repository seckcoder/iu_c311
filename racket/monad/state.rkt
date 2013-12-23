#lang racket

(require "utils.rkt")

; remove even member and count the number of even numbers
(define rember-even-count
  (lambda (l)
    (cond
      ((null? l) (unit '()))
      ((list? (car l))
       (bind
         (rember-even-count (car l))
         (lambda (a)
           (bind
             (rember-even-count (cdr l))
             (lambda (d)
               (unit (cons a d)))))))
      ((odd? (car l))
       (bind
         (rember-even-count (cdr l))
         (lambda (d)
           (unit (cons (car l) d)))))
      (else
        ; even
        (bind
          (rember-even-count (cdr l))
          (lambda (d)
            (lambda (s)
              `(,d . ,(+ 1 s))))))
      )))

; The state monad
; to maintain state-passing illusion, we need to pass the state in and out.
(define unit
  (lambda (l)
    (lambda (s)
      `(,l . ,s))))

(define (bind ma f)
  (lambda (s)
    (match (ma s)
      [(cons la new-s)
       ((f la) new-s)])))

((rember-even-count '((1 2) (3 4) 5 6 (7 8))) 0)
