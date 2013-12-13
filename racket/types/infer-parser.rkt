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
    (rand expression?))
  (if-exp
    (test expression?)
    (then expression?)
    (else expression?))
  (lambda-exp
    (var symbol?)
    (body expression?))
  (letrec-exp
    (p-name symbol?)
    (proc expression?)
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
    [`(if ,test ,then)
      (parse `(if ,test
                ,then
                (void)))]
    [`(if ,test ,then ,else)
      (if-exp (parse test)
              (parse then)
              (parse else))]
    ; lambda
    [`(lambda (,rand) ,body)
      (lambda-exp rand
                  (parse body))]
    [`(letrec ((,name ,proc)) ,body)
      (letrec-exp name
                  (parse proc)
                  (parse body))]
    ; procedure call
    [(list rand rator)
     (call-exp (parse rand)
               (parse rator))]
    ))
