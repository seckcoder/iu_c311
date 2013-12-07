#lang eopl

(require racket/match)
(require "../../base/utils.rkt")
(require "base.rkt")

(provide (all-defined-out))

(define-datatype
  simple-exp simple-exp?
  (cps-const-exp
    (cst const?))
  (cps-var-exp
    (var symbol?))
  (cps-quote-exp
    (exp sexp?))
  (cps-op-exp
    (op op?)
    (params (list-of simple-exp?)))
  (cps-lambda-exp
    (vars (list-of symbol?))
    (body tfexp?))
  )

(define-datatype
  tfexp tfexp?
  (simple-exp->exp
    (exp simple-exp?))
  (cps-if-exp
    (test simple-exp?)
    (then tfexp?)
    (else tfexp?))
  (cps-compound-exp
    (exps (list-of simple-exp?))
    (exp tfexp?))
  (cps-letrec-exp
    (p-names (list-of symbol?))
    (procs (list-of simple-exp?))
    (body tfexp?))
  (cps-call-exp
    (rand simple-exp?)
    (rators (list-of simple-exp?)))
  )

(define (parse1 sexp)
  (match sexp
    [(? const? x) (cps-const-exp x)]
    [(? symbol? x) (cps-var-exp x)]
    ; quoted sexp
    [`(quote ,x)
      (cps-quote-exp x)]
    ; builtin ops
    [(list (? op? op) params* ...)
     (cps-op-exp op (map parse1-or-fail params*))]
    ; lambda
    [`(lambda (,params ...) ,body ,bodies* ...)
      (cps-lambda-exp params
                  (parse `(begin ,body ,@bodies*)))]

    [_ #f]))

(define (parse1-or-fail sexp)
  (let ((v (parse1 sexp)))
    (if v
      v
      (eopl:error 'parse1-or-fail "~s is not a simple expression" sexp))))

(define (parse sexp)
  (let ((parse1-res (parse1 sexp)))
    (if parse1-res
      (simple-exp->exp parse1-res)
      (match sexp
        ; if 
        [`(if ,test ,then ,else)
          (cps-if-exp (parse1-or-fail test)
                      (parse then)
                      (parse else))]
        [`(if ,test ,then)
          (parse `(if ,test ,then (void)))]
        ; compound exp
        [`(begin ,body ,bodies* ...)
          (if (null? bodies*)
            (cps-compound-exp '()
                              (parse body))
            (cps-compound-exp (map parse1-or-fail 
                                   (cons body (drop-right bodies* 1)))
                              (parse (take-right bodies* 1))))]
        ; let
        [`(let ((,var ,val) ...) ,body ,bodies* ...)
          (parse `((lambda (,@var)
                     ,@(cons body bodies*))
                   ,@val))]
        ; letrec
        [`(letrec ((,name* ,proc*) ...) ,body ,bodies* ...)
          (cps-letrec-exp name*
                          (map parse1-or-fail proc*)
                          (parse `(begin ,body ,@bodies*)))]
        ; procedure call
        [(list rand rators ...)
         (cps-call-exp (parse1-or-fail rand)
                       (map parse1-or-fail rators))]
        ))))
