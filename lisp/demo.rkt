#lang racket

(define except-stack '())

(define extend-stack
  (lambda (name val st)
    (cons (list name val)
          st)))

(define top car)

(define pop-lookup-stack
  (lambda (name st)
    (cond ((null? st)
           (error 'pop-lookup-stack "exception :~s is not handled" name))
          ((eq? (car (top st)) name)
           (list st (cdr (top st))))
          (else
            (pop-lookup-stack name st)))))

(define (lookup! name)
  (match (pop-lookup-stack name except-stack)
    [(list st k)
     (set! except-stack st)
     k]))

(define (extend! name k)
  (set! except-stack (extend-stack name k except-stack)))

(define-syntax catch
  (syntax-rules()
    [(_ name handle body ...)
     (call/cc 

(define-syntax throw
  (syntax-rules()
    [(_ name msg)
     ((lookup! name) msg)]))

(define find-sym
  (lambda (id tree)
    (if (pair? tree)
      (or (find-sym id (car tree))
          (find-sym id (cdr tree)))
      (eq? tree id))))


(catch 'V-Eq-3
       (lambda (msg)
         (printf "Exception Happended:~s\n" msg))
       (find-sym 'id
