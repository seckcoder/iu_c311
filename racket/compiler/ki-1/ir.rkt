#lang racket

(require (prefix-in tr: "tree.rkt")
         "lang.rkt"
         "env.rkt")

(provide gen)

(define (e:trans env)
  (define (tr-exp ast)
    (match ast
      [(e:const v)
       (tr:const v)]
      [(e:var v)
       (let ((offset (h:apply env v)))
         (tr:mem (tr:temp 'fp)
                 offset))]
      [(e:biop op a b)
       (tr:biop op
                (tr-exp a)
                (tr-exp b))]
      [(e:unop op v)
       (tr:unop op (tr-exp v))]
      [(e:vec t vs)
       ...]
      [(e:vecref v i)
       ...]
      [(e:fun ft (e:fv v body))
       (
