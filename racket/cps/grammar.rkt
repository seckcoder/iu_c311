#lang eopl

(require racket/match)
(require "../base/utils.rkt")

(provide (all-defined-out))

(define atom?
  (lambda (v)
    (and (not (pair? v))
         (not (null? v)))))
         
(define const?
  (lambda (v)
    (or (number? v)
        (string? v)
        (boolean? v))))

(define op?
  (lambda (op)
    (memq op '(+ - * / = zero? cons car cdr list null? not
               number? symbol? list? eq? eqv? equal? void
               atom? void?))))

(define (parse-multi exps)
  (map (lambda (exp)
         (parse exp))
       exps))

(define-datatype
  simple-exp simple-exp?
  (const-exp
    (cst const?))
  (var-exp
    (var symbol?))
  (atom-exp
    (sym atom?))
  (list-exp
    (vals (list-of simple-exp?)))
  (op-exp
    (op op?)
    (params (list-of simple-exp?)))
  (lambda-exp
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
    [(? const? x) (const-exp x)]
    [(? symbol? x) (var-exp x)]
    ; a list(this should be put before symbol)
    [`(quote (,x* ...))
      (list-exp (map (lambda (x)
                       (cond ((symbol? x)
                              (parse1-or-fail `(quote ,x)))
                             ((list? x)
                              (parse1-or-fail (list 'quote x)))
                             (else
                               (parse1-or-fail x))))
                       x*))]
    ; quoted atom
    [`(quote ,x)
      (atom-exp x)]
    ; builtin ops
    [(list (? op? op) params* ...)
     (op-exp op (map parse1-or-fail params*))]
    ; lambda
    [`(lambda (,params ...) ,body ,bodies* ...)
      (lambda-exp params
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
