#lang eopl

"""
Scanner-spec ::= ({Regexp-and-action}*)
Regexp-and-action ::= (Name ({Regexp}*) Action)
Name ::= Symbol
Regexp ::= String | letter | digit | whitespace | any
       ::=(not Character)|(or {Regexp}*)
       ::=(arbno Regexp)|(concat {Regexp}*)
Action ::= skip | symbol | number | string
"""

; {x := foo; while x do x := (x - bar)}

; specification to used by scanner to generate tokens
(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

; Statement ::={Statement;Statement}
;           ::= while Expression do Statement
;           ::= Identifier := Expression
; Expression ::= Identifier
;            ::= (Expression - Expression)
(define grammar-al
  '((statement
      ("{" statement ";" statement "}")
      compound-statement)
    (statement
      ("while" expression "do" statement)
      while-statement)
    (statement
      (identifier "=" expression)
      assign-statement)
    (expression
      (identifier)
      var-exp)
    (expression
      ("(" expression "-" expression ")")
      diff-exp)
    )
  )


(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))


; (display (list-the-datatypes))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-a grammar-al))

; (display (just-scan "{x := foo}"))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))

(define code1 "{ x = foo ; while x do x = ( x - y ) }")
(scan&parse code1)
(define code2 " x = foo ")
(scan&parse code2)
