#lang racket

(provide op?)

(define op?
      (lambda (op)
        (memq op '(+ - * / = zero? cons car cdr list null? not
                     number? symbol? list? eq? eqv? equal? void
                     atom? void?))))
