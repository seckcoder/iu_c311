#lang eopl

(require racket/match)
(require "../../base/utils.rkt")
(require "base.rkt")

(provide (all-defined-out))

(define-datatype
  expression expression?
  (const-exp
    (cst const?))
  (var-exp
    (var symbol?))
  (quote-exp
    (exp sexp?))
  (op-exp
    (op op?)
    (params (list-of expression?)))
  (lambda-exp
    (vars (list-of symbol?))
    (body expression?))
  (if-exp
    (test expression?)
    (then expression?)
    (else expression?))
  (call-exp
    (rand expression?)
    (rators (list-of expression?)))
  )


(define parse
  (lambda (sexp)
    (match sexp
      [(? const? x) (const-exp x)]
      [(? symbol? x) (var-exp x)]
      [`(quote ,x)
        (quote-exp x)]
      [(list (? op? op) params* ...)
       (op-exp op (map parse params*))]
      [`(lambda (,params ...) ,body)
        (lambda-exp params
                    (parse body))]
      [`(if ,test ,then ,else)
        (if-exp (parse test)
                (parse then)
                (parse else))]
      [`(if ,test ,then)
        (parse `(if ,test ,then (void)))]
      [(list rand rators ...)
       (call-exp (parse rand)
                 (map parse rators))]
      )))
