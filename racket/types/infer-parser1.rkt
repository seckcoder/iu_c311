#lang racket

(require eopl/datatype
         "../base/utils.rkt"
         "../cps/builtin.rkt")

(provide (all-defined-out))

(define-datatype
  expression expression?
  (const-exp
    (cst const?))
  (var-exp
    (var symbol?))
  (quote-exp
    (sexp sexp?))
  (op-exp
    (op op?)
    (rands (list-of expression?)))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  (if-exp
    (test expression?)
    (then expression?)
    (else expression?))
  (lambda-exp
    (vars (list-of symbol?))
    (body expression?))
  (letrec-exp
    (p-names (list-of symbol?))
    (procs (list-of expression?))
    (body expression?))
  )


(define (single-or-compound exps)
  (if (null? (cdr exps))
    (car exps)
    `(begin
       ,@exps)))

(define (parse sexp)
  (match sexp
    [(? const? x) (const-exp x)]
    [(? symbol? x) (var-exp x)]
    ; symbol
    [`(quote ,x) (quote-exp x)]
    ; builtin ops
    [(list (? op? op) params ...)
     (op-exp op (map parse params))]
    ; if 
    [`(if ,test ,then ,else)
      (if-exp (parse test)
              (parse then)
              (parse else))]
    ; lambda
    [`(lambda (,params ...) ,body)
      (lambda-exp params
                  (parse body))]
    [`(letrec ((,names ,procs) ...) ,body)
      (letrec-exp names
                  (map parse procs)
                  (parse body))]
    ; procedure call
    [(list rator rands ...)
     (call-exp (parse rator)
               (map parse rands))]
    ))
