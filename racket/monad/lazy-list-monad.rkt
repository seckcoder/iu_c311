#lang racket


; Three methods to construct monad: unit, mzero and cons
(define (unit a)
  (list a))

(define (mzero)
  '())

;cons

(define (bind ma f)
  (match ma
    [(list)
     (mzero)]
    [(list a)
     (f a)]
    [(cons a get-ma)
     (mplus (f a)
            (lambda ()
              (bind (get-ma)
                    f)))]))

;ma * get-ma -> mb
(define (mplus ma get-ma)
  (match ma
    [(list)
     (get-ma)]
    [(list a)
     (cons a get-ma)]
    [(list a get-ma0)
     (cons a
           (lambda () (mplus (get-ma0) get-ma)))]))

(define (mmap n p ma)
  ;(printf "~a\n" ma)
  (match ma
    [(list)
     (mzero)]
    [(list a)
     (unit (p a))]
    [(cons a get-ma)
     (cons (p a)
           (cond
             ((= n -1) (mmap n p (get-ma)))
             ((> n 1) (mmap (- n 1) p (get-ma)))
             (else '())))]))

(define (mmap-inf p ma)
  (mmap -1 p ma))

(define (mtake n ma)
  (mmap n (lambda (v) v) ma))

; lazy version of assoc
(define (s-assoc l v)
  (cond
    ((null? l) (mzero))
    ((eq? (car (car l)) v)
     (mplus (unit (car l))
            (lambda ()
              (s-assoc (cdr l) v))))
    (else
      (s-assoc (cdr l) v))))

(mtake 1 (s-assoc '((1 . 'a)
                    (2 . 'b)
                    (3 . 'c)
                    (2 . 'd)
                    (2 . 'e))
                  2))
