#lang eopl

(provide (all-defined))

(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))


(define grammar-al
  '((program
      (expression)
      a-program)
    (expression
      (number)
      const-exp)
    (expression
      ("-(" expression "," expression ")")
      diff-exp)
    (expression
      ("zero?" "(" expression ")")
      zero?-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      (identifier)
      var-exp)
    (expression
      ("let" identifier "=" expression "in" expression)
      let-exp)
    (expression
      ("proc" "(" identifier ")" expression)
      proc-exp)
    (expression
      ("(" expression expression ")")
      call-exp)
    (expression
      ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
      letrec-exp)
    (expression
      ("cons(" expression "," expression ")")
      cons-exp)
    (expression
      ("emptyList")
      empty-lst-exp)
    (expression
      ("car(" expression ")")
      car-exp)
    (expression
      ("cdr(" expression ")")
      cdr-exp)
    (expression
      ("null?(" expression ")")
      is-empty-exp)
    (expression
      ("list(" (arbno expression) ")")
      list-exp)
    (expression
      ("{" (arbno expression ";") "}")
      compound-exp)
    (expression
      ("set" identifier "=" expression)
      set-exp)
    (expression
      ("*(" expression "," expression ")")
      mult-exp)
    ))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))

(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))
