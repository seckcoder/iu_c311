#lang racket
; a light-weight scheme parser based on pattern matching and algebraic data type

(require racket/match)
(require eopl/datatype)
(require "base/utils.rkt")

(define const?
  (lambda (v)
    (or (number? v)
        (string? v))))

(define op?
  (lambda (op)
    (memq op '(+ - * / zero? cons car cdr list null?
               number? symbol? list?))))

(define-datatype
  expression expression?
  (const-exp
    (cst const?))
  (var-exp
    (var symbol?))
  (symbol-exp
    (sym symbol?))
  (op-exp
    (op op?)
    (params (list-of expression?)))
  (call-exp
    (rand expression?)
    (rators (list-of expression?)))
  (if-exp
    (test expression?)
    (then expression?)
    (else expression?))
  (lambda-exp
    (vars (list-of symbol?))
    (body expression?))
  (compound-exp
    (exps (list-of expression?)))
  (define-exp
    (var symbol?)
    (val expression?))
  (list-exp
    (vals (list-of expression?)))
  (letrec-exp
    (p-names (list-of symbol?))
    (procs (list-of expression?))
    (body expression?))
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
    ; a list(this should be put before symbol)
    [`(quote (,x* ...))
      (list-exp (parse-multi x*))]
    ; symbol
    [`(quote ,x) (symbol-exp x)]
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
    ; how to support define in any order?
    [`(define ,var ,val)
      (define-exp var (parse val))]
    [`(let ((,var ,val) ...) ,body ,bodies* ...)
      (parse `((lambda (,@var)
                 ,@(cons body bodies*))
               ,@val))]
    [`(letrec ((,name* ,proc*) ...) ,body ,bodies* ...)
      (letrec-exp name*
                  (parse-multi proc*)
                  (parse `(begin ,body ,@bodies*)))]
    ; procedure call
    [(list rand rators ...)
     (call-exp (parse rand)
               (map (lambda (rator)
                      (parse rator))
                    rators))]
    ))
#|(parse '(lambda (a b c)
          (display a)
          (foo b)))

(parse '(define a 3))

(parse '((lambda (a b)
           (display a)
           (display b)) 3 4))

(parse '(let ((a 3)
              (b 4))
          (display a)
          (display b)))

(parse '(cons a '(1 2 3)))|#

#|(parse '(letrec ((foo (lambda (v)
                        v))
                 (bar (lambda (v)
                        v)))
          (foo 2)
          (bar 3)))|#
