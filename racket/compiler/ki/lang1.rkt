; Identifier: [a-zA-Z][0-9a-zA-Z]*
; Integer
; Binary operators: && < + - *
; Comments: /**/, //
; mj is short for mini-java
(define mj-lexer
  (lexer
    ; keyword
    [keyword
      `(keyw ,lexeme)]
    [punct
      `(punct ,lexeme)]
    ; identifier
    [(:: alphabetic
         (:*
           (:or (:/ #\0 #\9)
                alphabetic
                #\_)))
     `(id ,lexeme)]
    ; integer
    [(:: (:? #\-) (:+ (:/ #\0 #\9)))
      `(int ,(string->number lexeme))]
    ; operators
    [(:or "&&" "||" "<=" ">=" #\> #\< #\+ #\- #\* #\= "==")
     `(op ,(string->symbol lexeme))]
    ; comment
    [(:: "//" any-string #\newline)
     (mj-lexer input-port)]
    ; comments
    [(:: "/*" (complement (:: any-string "*/" any-string)) "*/")
     (mj-lexer input-port)]
    ; whitespace
    [whitespace
     (mj-lexer input-port)]
    ))
