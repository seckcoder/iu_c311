#lang racket

(require "parsing-table.rkt"
         "demos.rkt"
         "ds.rkt")

; top down parsing

(define (make-parser grammar)
  (let ((parsing-tbl (build-parsing-table grammar)))
    (define (token-ended? tokens)
      (equal? tokens '($)))
    (define (parse es tokens)
      (cond
        ((and (null? es)
              (token-ended? tokens))
         'finished)
        ((null? es)
         (error 'parse "failed"))
        (else
          (let ((e (car es))
                (t (car tokens)))
            (if (terminal? e)
              (if (eq? e t)
                (parse (cdr es)
                       (cdr tokens))
                (error 'parse "failed"))
              (parse
                (append
                  (filter
                    (lambda (v)
                      (not (eq? v 'sigma)))
                    (apply-table2d parsing-tbl e t))
                  (cdr es))
                tokens))))))
    parse))

(define parse (make-parser (map make-prod arithmetic-prods)))
(parse '(E) (list 'number '$))
