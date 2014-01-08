#lang racket

(require racket/match)
(require racket/pretty)

(provide anything?
         list-of
         index-of
         check
         println
         while
         add1
         sub1
         caddddr
         find
         mfindf
         mapn
         v->lst
         atom?
         const?
         sexp?
         tail
         flatmap
         safe-take
         apply-base-ns
         sym-append
         combine
         allf
         anyf
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
         (error 'check "~a is not ~a to ~a\n\t~a not ~a ~a\n" `a `pred `b va `pred vb)
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

(define (v->lst n v)
  (mapn (lambda (_) v) n))

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

(define tail
  (lambda (lst)
    (cond ((null? lst)
           (error 'tail "list is null"))
          ((null? (cdr lst))
           (car lst))
          (else
            (tail (cdr lst))))))

(define mfindf
  (lambda (handle lst)
    (match (find handle lst)
      [(list finded? rest ...)
       finded?])))

(define flatmap
  (lambda (handle . rest)
    (apply foldl `(,(lambda args
                      (match args
                        [(list handle-params ... acc)
                         (append acc
                                 (apply handle handle-params))]))
                    ()
                    ,@rest))))

(define safe-take
  (lambda (lst n)
    (if (or (= n 0)
            (null? lst))
      '()
      (cons (car lst)
            (safe-take (cdr lst)
                       (- n 1))))))

(define (apply-base-ns op rands)
  (apply (eval op (make-base-namespace))
         rands))

(define sym-append
  (lambda syms
    (string->symbol
      (foldl
        (lambda (sym s)
          (string-append
            s
            (symbol->string sym)))
        ""
        syms))))

(define combine
  (match-lambda*
    [(list f) f]
    [(list f f* ...)
     (lambda (v)
       ((apply combine f*) (f v)))]))

(define allf
  (match-lambda*
    [(list) (lambda (v) #t)]
    [(list f0 f* ...)
     (lambda (v)
       (and (f0 v)
            ((apply allf f*) v)))]))

(define anyf
  (match-lambda*
    [(list) (lambda (v) #f)]
    [(list f0 f* ...)
     (lambda (v)
       (or (f0 v)
           ((apply anyf f*) v)))]))

(module+ test
  (require rackunit)
  ((combine (lambda (v) v)
            (lambda (v) v)
            (lambda (v) v))
   3)
  ((allf (lambda (v) (> v 0))
         (lambda (v) (< v 10))
         (lambda (v) (even? v))) 4)
  ((anyf (lambda (v) (= v 1))
         (lambda (v) (= v 2))
         (lambda (v) (= v 3))) 3)
  ((anyf (lambda (v) (= v 1))
         (lambda (v) (= v 2))
         (lambda (v) (= v 3))) 0)
  )
