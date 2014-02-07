#lang racket

(require (prefix-in tr: "tree.rkt")
         "lang.rkt"
         "env.rkt")

(provide gen)

(define (e:trans env)
  (define (tr-seq exps)
    (cond
      [(null? exps)
       (error 'tr-seq "empty body")]
      [(null? (cdr exps))
       (tr-exp (car exps))]
      [else
        (eseq (exp (tr-exp (car exps)))
              (tr-seq (cdr exps)))]))
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
      [(e:seq exps)
       (tr-seq! exps)]
      [(e:set v val)
       (tr:move (tr-exp v)
                (tr-exp val))]
      [(e:vecset v i val)
       ...]
      [(ife test then else)
       (tr:cjump (tr-exp test)
              (tr-exp then)
              (tr-exp else))]
      [(lete decls body)
       (let ((venv (d:trans venv decls)))
         ((e:trans venv) body))]
      [(app rator rands)
       ? 
       (call rator rands)]
      ))
  )
