#lang racket

; specify grammars

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))


; Identifier: [a-zA-Z][0-9a-zA-Z]*
; Integer
; Binary operators: && < + - *
; Comments: /**/, //
; mj is short for mini-java
(define mj-lexer
  (lexer
    [(:: alphabetic
         (:*
           (:or (:/ #\0 #\9)
                alphabetic
                #\_)))
     `(id ,lexeme)]
    [(:: (:? #\-) (:+ (:/ #\0 #\9)))
      `(int ,(string->number lexeme))]
    [(:or "&&" #\< #\+ #\- #\*)
     `(op ,(string->symbol lexeme))]
    [(:: "//" any-string #\newline)
     (mj-lexer input-port)]
    [(:: "/*" (complement (:: any-string "*/" any-string)) "*/")
     (mj-lexer input-port)]
    [whitespace
     (mj-lexer input-port)]
    ))

(define (lexer-booster a-lexer port)
  (let loop ((tokens '()))
    (let ((token (a-lexer port)))
      (if (eq? token 'eof)
        (reverse tokens)
        (loop (cons token tokens))))))


(module+ test
  ; test lexer
  (require rackunit)
  (check-equal?
    (lexer-booster mj-lexer (open-input-string "aBc 4 + /*x//x*/ //aaa/*bcd*/
                                               cde"))
    '((id "aBc") (int 4) (op +) (id "cde"))))
 
