#lang eopl
; the first part is in c5_43
; the second part: translator is in this file

; (letcc k body ...+) ->
; (call/cc (lambda (k) body ...+))

(require racket/match)

(define translator
  (lambda (exp)
    (match exp
      [(list 'letcc k body)
       `(call/cc (lambda (,k)
                   ,(translator body)))]
      [(list rator rand)
       `(,rator ,(translator rand))]
      [(var v)
       (if (or (number? v)
               (symbol? v))
         v
         (eopl:error 'translator "pattern not matched"))]
      )))

(define d-t
  (lambda (p)
    (display (translator p))(newline)))

(d-t '(letcc k
        (k 2)))
