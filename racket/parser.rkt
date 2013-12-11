#lang racket

(require eopl/datatype
         "base/utils.rkt")

(provide (all-defined-out))

(define op?
  (lambda (op)
    (memq op '(+ - = * / zero? cons car cdr list null?
               number? symbol? list?))))

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
    (params (list-of expression?)))
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
  (compound-exp
    (exps (list-of expression?)))
  (letrec-exp
    (p-names (list-of symbol?))
    (procs (list-of expression?))
    (body expression?))
  (letcc-exp
    (var symbol?)
    (body expression?))
  (set-exp
    (var symbol?)
    (val expression?))
  (define-exp
    (var symbol?)
    (val expression?))
  )

(define (parse-multi exps)
  (map (lambda (exp)
         (parse exp))
       exps))

(define (parse sexp)
  (match sexp
    [(? number? x) (const-exp x)]
    [(? string? x) (const-exp x)]
    [(? symbol? x) (var-exp x)]
    ; symbol
    [`(quote ,x) (quote-exp x)]
    ; builtin ops
    [(list (? op? op) params* ...)
     (op-exp op (parse-multi params*))]
    ; if 
    [`(if ,test ,then ,else)
      (if-exp (parse test)
              (parse then)
              (parse else))]
    ; lambda
    [`(lambda (,params ...) ,body ,bodies* ...)
      (lambda-exp params
                  (parse `(begin ,body ,@bodies*)))]
    [`(begin ,body ,bodies* ...)
      (compound-exp (parse-multi (cons body bodies*)))]
    [`(let ((,var ,val) ...) ,body ,bodies* ...)
      (parse `((lambda (,@var)
                 ,@(cons body bodies*))
               ,@val))]
    [`(letrec ((,name* ,proc*) ...) ,body ,bodies* ...)
      (letrec-exp name*
                  (parse-multi proc*)
                  (parse `(begin ,body ,@bodies*)))]
    [`(let/cc ,var ,body ,bodies* ...)
      (letcc-exp var
                 (parse `(begin ,body ,@bodies*)))]
    [`(set! ,var ,val)
      (set-exp var
               (parse val))]
    [`(define ,var ,val)
      (define-exp var (parse val))]
    ; procedure call
    [(list rand rators ...)
     (call-exp (parse rand)
               (map (lambda (rator)
                      (parse rator))
                    rators))]
    ))
