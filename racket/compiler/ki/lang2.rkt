#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out))

(define-tokens a (num var))
(define-empty-tokens b (+ - * = == != < lpar rpar lbracket rbracket lbrace rbrace dot sem comma
                          class public static void main String extends
                          return int bool if else while true false this new
                          eof))

; lex
(define-lex-abbrevs
  [identifier (:: alphabetic
                  (:*
                    (:or (:/ #\0 #\9)
                         alphabetic
                         #\_)))]
  [integer (:: (:? #\-) (:+ (:/ #\0 #\9)))]
  [comment (:or (:: "//" any-string #\newline)
                (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))]
  )

(define mj-lexer
  (lexer-src-pos
    ["+" (token-+)  ]
    ["-" (token--)  ]
    ["*" (token-*)  ]
    [identifier (token-var lexeme)]
    [integer (token-num lexeme)]
    [comment (mj-lexer input-port)]
    [whitespace
     (return-without-pos (mj-lexer input-port))]
    [(eof) (token-eof)]
    ))

(define (lexer-booster a-lexer port)
  (let loop ((tokens '()))
    (let ((token (a-lexer port)))
      (if (eq? (position-token-token token) 'eof)
        (reverse tokens)
        (loop (cons token tokens))))))


(module+ test
  ; test lexer
  (require rackunit)
  (define (print-tokens tokens)
    (for-each
      print-token
      tokens))
  (define (print-token token)
    (print token)
    (newline))
    #|(match token
      [(position-token tok start-pos end-pos)
       (print tok)
       (newline)]))|#
  (print-tokens
    (call-with-input-file "demo/d3.java"
                          (lambda (port)
                            (port-count-lines! port)
                            (lexer-booster mj-lexer port))))
  )

; grammar

(define report-err
  (match-lambda*
    [(list tok-ok? tok-name tok-value)
     (printf "~a ~a ~a\n" tok-ok? tok-name tok-value)]
    [(list tok-ok? tok-name tok-value start-pos end-pos)
     (printf "~a ~a ~a ~a ~a\n" tok-ok? tok-name tok-value start-pos end-pos)]))

(struct prog (main clses))
(struct mainc (name arg stms))
(struct cls (name base vars methods))
(struct var-decl (type var))
(struct method-decl (ret-type name args vars stms ret))
(struct compound-stms (stms))
(struct if-stms (test then else))
(struct while-stms (pred stm))
(struct set-stms (var exp))
(struct Type ())
(struct Array Type (t))
(struct Bool Type ())
(struct Int Type ())
(struct expression ())
(struct op-exp expression (op e1 e2))
(struct const-exp expression (v))

(define mj-parser
  (parser
    (start Program)
    (end eof)
    (error report-err)
    (tokens a b)
    (src-pos)
    (precs (right =)
           (nonassoc == !=)
           (left - +)
           (left *))
    (grammar
      (Program ((MainClass ClassDecls)
                (prog $1 $2)))
      (MainClass ((class var lbrace public static void main lpar Arg rpar
                         lbrace Statements rbrace)
                  (mainc $2 $9 $12)))
      (ClassDecl ((class var lbrace VarDecls MethodDecls)
                  (cls $2 'Object $4 $5))
                 ((class var extends var lbrace VarDecls MethodDecls)
                  (cls $2 $4 $6 $7)))
      (ClassDecls (()
                   '())
                  ((ClassDecl ClassDecls)
                   (cons $1 $2))
                  )
      (VarDecl ((Type var sem)
                (var-decl $1 $2)))
      (VarDecls (()
                 '())
                ((VarDecl VarDecls)
                  (cons $1 $2)))
      (MethodDecls ((public Type var lpar Args rpar
                            lbrace VarDecls Statements return Exp sem rbrace)
                    (method-decl $2 $3 $5 $8 $9 $11)))
      (Arg ((VarDecl)
            $1))
      (Args (()
             '())
            ((VarDecl)
             (list $1))
            ((VarDecl comma Args)
             (cons $1 $3)))
      (Type ((Type lbracket rbracket)
             (Array $1))
            ((bool)
             (Bool))
            ((int)
             (Int))
            ((var)
             $1))
      (Statement ((If-s)
                  $1)
                 ((While-s)
                  $1)
                 ((Set-s)
                  $1)
                 ((lbrace Statements rbrace)
                  (compound-stms $2)))
      (Statements (()
                   '())
                  ((Statement)
                   (list $1))
                  ((Statement sem Statements)
                   (cons $1 $3)))
      (If-s ((if lpar Exp rpar Statement else Statement)
             (if-stms $3 $5 $7)))
      (While-s ((while lpar Exp rpar Statement)
                (while-stms $3 $5)))
      (Set-s ((var = Exp sem)
              (set-stms $1 $3)))
      (Exp ((Exp + Exp)
            (op-exp '+ $1 $3))
           ((Exp - Exp)
            (op-exp '- $1 $3))
           ((num)
            (const-exp $1))
           ((true)
            (const-exp #t))
           ((false)
            (const-exp #f)))
      )))

(define (lex-this lexer input)
  (lambda ()
    (lexer input)))

(module+ test-
  (call-with-input-file "demo/d2.java"
                        (lambda (port)
                          (mj-parser (lex-this mj-lexer port))))
  )
