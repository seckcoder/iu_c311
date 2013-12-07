#lang eopl

(require racket/base)
(require racket/match)
(require racket/list)
(require racket/pretty)

(provide anything?
         list-of
         index-of
         check
         println
         foldl
         while
         add1
         sub1
         caddddr
         gensym
         find
         filter
         make-hasheq
         hash-ref!
         mapn
         drop-right
         take-right
         print
         printf
         atom?
         const?
         sexp?
         andmap
         splitf-at
         let-values
         takef
         dropf
         pretty-print
         )

(define anything?
  (lambda (v)
    #t))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define index-of
  (lambda (lst v)
    (define (iter lst v idx)
      (cond ((null? lst) -1)
            ((eq? (car lst) v) idx)
            (else
              (iter (cdr lst) v (+ 1 idx)))))
    (iter lst v 0)))

(define-syntax check
  (syntax-rules ()
    [(_ pred a b)
     (let ((va a)
           (vb b))
       (if (not (pred va vb))
         (eopl:error 'check "~s:~s not ~s ~s:~s" `a va `pred `b vb)
         'ok))]))

(define list-n
  (lambda (n v)
    (if (= n 0)
      '()
      (cons v (list-n (- n 1) v)))))

(define-syntax while
  (syntax-rules ()
    [(_ pred body body* ...)
     (let loop ((v 'unintialized))
       (if pred
         (let ((v (let ()
                    body body* ...)))
           (loop v))
         v))]))

(define caddddr
  (lambda (lst)
    (car (cddddr lst))))

(define find
  (lambda (handle lst)
    (let loop ((lst lst)
               (idx 0))
      (cond ((null? lst) (list #f '() idx))
            ((handle (car lst)) (list #t (car lst) idx))
            (else
              (loop (cdr lst) (add1 idx)))))))

(define mapn
  (lambda (handle n)
    (let loop ((i 0))
      (if (>= i n)
        '()
        (cons (handle i)
              (loop (add1 i)))))))

(define atom?
  (lambda (v)
    (and (not (pair? v))
         (not (null? v)))))


(define const?
  (lambda (v)
    (or (number? v)
        (string? v)
        (boolean? v))))


(define sexp?
  (lambda (s)
    (or (atom? s)
        (list? s))))

(define println
  (lambda args
    (apply print args)(newline)))
