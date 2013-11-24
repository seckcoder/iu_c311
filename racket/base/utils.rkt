#lang eopl

(require racket/base)

(provide anything?
         list-of
         index-of
         check
         println
         foldl
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
     (if (not (pred a b))
       (eopl:error 'check "~s:~s not ~s ~s:~s" `a a `pred `b b)
       'ok)]))

(define println
  (lambda args
    (apply eopl:printf args)
    (newline)))

(define list-n
  (lambda (n v)
    (if (= n 0)
      '()
      (cons v (list-n (- n 1) v)))))
