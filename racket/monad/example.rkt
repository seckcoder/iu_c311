#lang racket
; id monad

; return: a -> ma
; bind:  ma * (a -> mb) -> mb
(module id racket
  (define return (lambda (a) a))
  (define bind (lambda (ma f) (f ma)))
  ; monadic plus
  (define plus
    (lambda (a b)
      (bind
        (return (+ a b)) ; ma
        (lambda (x) (return x)) ; a -> mb
        )))
  )

(module id1 racket
  (define bind
    (lambda (ma sequel)
      (sequel ma)))
  (define unit
    (lambda (a)
      a))
  (bind (unit (printf "One\n"))
        (lambda (_)
          (unit (printf "Two\n"))))
  (bind (unit 5)
        (lambda (x)
          (unit (+ 3 x))))
  )

(define rem-count-evens
  (lambda (l)
    (cond
      ((null? l) `(,l . 0))
      ((list? (car l))
       (match (rem-count-evens (car l))
         [(cons l1 n1)
          (match (rem-count-evens (cdr l))
            [(cons l2 n2)
             (cons (cons l1 l2)
                   (+ n1 n2))])]))
      ((odd? (car l))
       (match (rem-count-evens (cdr l))
         [(cons l1 n1)
          (cons (cons (car l) l1) n1)]))
      (else
        (match (rem-count-evens (cdr l))
          [(cons l1 n1)
           (cons l1 (+ 1 n1))]))
      )))

;(rem-count-evens '((1 2 3) 4 6 7 (2 9 83 35)))

(define remberevens-pure
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l))
       (cons (remberevens-pure (car l))
             (remberevens-pure (cdr l))))
      ((odd? (car l))
       (cons (car l)
             (remberevens-pure (cdr l))))
      (else (remberevens-pure (cdr l))))))

#|(define unit (lambda (a) ma))
(define f (lambda (a) b))

(define sequel
  (lambda (a)
    (unit (f a))))

(define star
  (lambda (sequel)
    (lambda (ma)
      mb)))|#


(module state racket
  (provide rember-even)
  (define unit
    (lambda (a)
      (lambda (s)
        `(,a . ,s))))

  (define star
    (lambda (sequel)
      (lambda (ma)
        (lambda (s)
          (match (ma s)
            [(cons new-a new-s)
             (let ((mb (sequel new-a)))
               (mb new-s))])))))

  #|(define rember-even
      (lambda (l)
        (cond
          ((null? l)
           (unit (cons '() 0)))
          ((list? (car l))
           ((star (lambda (a)
                    ((star (lambda (d)
                             (unit (cons (cons (car a)
                                               (car d))
                                         (+ (cdr a)
                                            (cdr d))))))
                     (rember-even (cdr l)))))
            (rember-even (car l))))
          ((odd? (car l))
           ((star (lambda (d)
                    (unit (cons (cons (car l)
                                      (car d))
                                (cdr d)))))
            (rember-even (cdr l))))
          (else 
            ((star (lambda (d)
                     (unit (cons (car d)
                                 (+ 1 (cdr d))))))
             (rember-even (cdr l)))))))|#

  (define rember-even
    (lambda (l)
      (cond
        ((null? l)
         (unit '()))
        ((list? (car l))
         ((star (lambda (a)
                  ((star (lambda (d)
                           (unit (cons a d))))
                   (rember-even (cdr l)))))
          (rember-even (car l))))
        ((odd? (car l))
         ((star (lambda (d)
                  (unit (cons (car l)
                              d))))
          (rember-even (cdr l))))
        (else
          ((star (lambda (d)
                   (rember-even (cdr l))))
           (lambda (s)
             `(_ . ,(+ 1 s))))))
      ))
  )

(require 'state)

((rember-even '((1 2 3) 4 6 7 (2 9 83 35))) 0)
