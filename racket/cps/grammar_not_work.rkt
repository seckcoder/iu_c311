#lang eopl

(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define grammar-al
  '((program
      (tfexp)
      a-program)
    (simple-exp
      (number)
      const-exp)
    (simple-exp
      (identifier)
      var-exp)
    (simple-exp
      ("(-" simple-exp  simple-exp ")")
      cps-diff-exp)
    (simple-exp
      ("(zero?" simple-exp ")")
      cps-zero-exp)
    (simple-exp
      ("(cons" simple-exp simple-exp ")")
      cps-cons-exp)
    (simple-exp
      ("(null?" simple-exp ")")
      cps-null-exp)
    (simple-exp
      ("(car" simple-exp ")")
      cps-car-exp)
    (simple-exp
      ("emptyList")
      cps-emptylist-exp)
    (simple-exp
      ("(list" (arbno simple-exp) ")")
      cps-list-exp)
    (simple-exp
      ("(number?" simple-exp ")")
      cps-number?-exp)
    (simple-exp
      (lambda-exp)
      cps-proc-exp)
    (simple-exp
      ("(define" identifier simple-exp ")")
      cps-define-exp)
    (lambda-exp
      ("(lambda" "(" (arbno identifier) ")" tfexp ")")
      cps-lambda-exp)
    (simple-exp
      ("(print" simple-exp ")")
      cps-print-exp)
    (tfexp
      (simple-exp)
      simple-exp->exp)
    (tfexp
      ("(let" "((" identifier  simple-exp "))"
       tfexp ")")
      cps-let-exp)
    (tfexp
      ("(letrec" "((" identifier lambda-exp ")"
                   (arbno "(" identifier lambda-exp ")")")"
       tfexp ")")
      cps-letrec-exp)
    (tfexp
      ("(if" simple-exp tfexp tfexp ")")
      cps-if-exp)
    (tfexp
      ("(" simple-exp (arbno simple-exp) ")")
      cps-call-exp)
    ))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))

(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))

(provide (all-defined-out))
