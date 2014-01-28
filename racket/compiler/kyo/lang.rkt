#lang racket

(require "../../base/utils.rkt")

(provide parse
         parse-exp
         parse-decl
         parse-ty
         Program
         (all-from-out 'Type)
         (all-from-out 'Decl)
         (all-from-out 'Exp)
         )

; parser for kyo

(module Type racket
  (provide (prefix-out t: (all-defined-out)))
  (struct Int () #:transparent)
  (struct Str () #:transparent)
  (struct Bool () #:transparent)
  (struct Record (fs) #:transparent)
  (struct Vec (t) #:transparent)
  ; no type
  (struct Unit () #:transparent)
  ; any type
  (struct Any () #:transparent)
  ; a placeholder; used for recursive types
  (struct Name (n t) #:transparent)
  ; rand and return
  (struct Ft (rnd rt) #:transparent)
  )

(require 'Type)

(define (parse-ty t)
  (match t
    ['int (t:Int)]
    ['str (t:Str)]
    ['bool (t:Bool)]
    ['unit (t:Unit)]
    [(? symbol? t)
     ; t is a type-id, it refers to some unknown type
     (t:Name t (ref (None)))]
    [`(vec ,type-id)
      (t:Vec type-id)]
    [`(,t1 -> ,t2)
      (t:Ft (parse-ty t1)
             (parse-ty t2))]
    [`(,t1 -> ,t2 ...)
      (t:Ft (parse-ty t1)
             (parse-ty t2))]
    [`((,id* ,type-id*) ...)
      (t:Record (map list id* type-id*))]
    ))

(module Decl racket
  (provide (prefix-out d: (all-defined-out)))
  (struct tydec (id t) #:transparent)
  (struct vardec (id t v) #:transparent)
  ; f is e:fun
  (struct fundec (id f) #:transparent)
  )
      
(require 'Decl)

(define (fn? f)
  (and (pair? f)
       (eq? (car f) 'fn)))

; return (list type value)
(define (parse-decl decl)
  (match decl
    [`(type ,type-id ,ty)
      (d:tydec type-id (parse-ty ty))]
    [`(def ,v : ,type-id ,val)
      (d:vardec v
                (parse-ty type-id)
                (parse-exp val))]
    [`(def ,v ,(? fn? f))
      (d:fundec v
                (parse-exp f))]
    ))

(define (parse-binding binding)
  (match binding
    [`(,v : ,type-id ,val)
      (parse-decl `(def ,v : ,type-id ,val))]
    [`(,v ,(? fn? f))
      (parse-decl `(def ,v ,f))]))

(define (parse-decls decls)
  (map parse-decl decls))

(module Exp racket
  (provide (prefix-out e: (all-defined-out)))
  (struct const (v) #:transparent)
  (struct var (v) #:transparent)
  (struct biop (op a b) #:transparent)
  (struct unop (op v) #:transparent)
  (struct vec (t vs) #:transparent)
  (struct vecref (v i) #:transparent)
  ;(struct fun (v body) #:transparent)
  (struct fun (t v) #:transparent)
  ; function value
  (struct fv (v body) #:transparent)
  (struct seq (exps) #:transparent)
  (struct set (v val) #:transparent)
  (struct vecset (v i val) #:transparent)
  (struct ife (test then else) #:transparent)
  (struct lete (decls body) #:transparent)
  (struct app (rator rand) #:transparent)
  )

(require 'Exp)

(define const? (anyf number? string? boolean?))

(define (parse-body body)
  (match body
    [(list)
     (error 'body "empty body")]
    [(list exp)
     (parse-exp exp)]
    [(list exp0 exp* ...)
     (parse-exp
       `(seq
          ,exp0
          ,@exp*))]))

(define (bi-op? op)
  (memq op '(+ - * and or = < > <= >= )))
(define (un-op? op)
  (memq op '(not)))

(define (parse-exp exp)
  (match exp
    [(? const? v)
     (e:const v)]
    [(? symbol? v)
     (e:var v)]
    [(list (? bi-op? op) a b)
     (e:biop op
             (parse-exp a)
             (parse-exp b))]
    [(list (? un-op? op) v)
     (e:unop op
             (parse-exp v))]
    [`(fn () : ,ret-type-id ,body ...)
      (e:fun (t:Ft (t:Unit)
                   (parse-ty ret-type-id))
             (e:fv (None)
                   (parse-body body)))]
    [`(fn ((,v : ,type-id)) : ,ret-type-id ,body ...)
      (e:fun (t:Ft
               (parse-ty type-id)
               (parse-ty ret-type-id))
             (e:fv (Some v)
                   (parse-body body)))]
    [`(fn ([,v0 : ,type-id0]
           [,v* : ,type-id*] ...) : ,ret-type-id
        ,body ...)
      (match (parse-exp
               `(fn ,(map (lambda (v type-id)
                            `(,v : ,type-id))
                          v*
                          type-id*)
                  : ,ret-type-id
                  ,@body))
        [(e:fun ft fv)
         (e:fun (t:Ft (parse-ty type-id0)
                      ft)
                (e:fun (Some v0)
                       fv))]
        )]
    [`(vec ,type-id ,v* ...)
      (e:vec (parse-ty type-id)
             (map parse-exp v*))]
    [`(vec-ref ,v ,i)
      (e:vecref (parse-exp v)
                (parse-exp i))]
    [`(seq ,exp0 ,exp* ...)
      (e:seq (map parse-exp (cons exp0 exp*)))]
    [`(set! ,v ,val)
      (e:set v (parse-exp val))]
    [`(vec-set! ,vec ,i ,val)
      (e:vecset (parse-exp vec)
                (parse-exp i)
                (parse-exp val))]
    [`(if ,test ,then ,else)
      (e:ife (parse-exp test)
             (parse-exp then)
             (parse-exp else))]
    [`(let (,bindings ...) ,body ...)
      (e:lete (map parse-binding bindings)
              (parse-body body))]
    [`(,rator ,rand)
      (e:app (parse-exp rator)
             (parse-exp rand))]
    ))

(struct Program (decls exp) #:transparent)
(define (parse decls exp)
  (Program (parse-decls decls)
           (parse-exp exp)))

(module+ test
  (parse (list
           '(def f (fn ([v : int]) : (vec int)
                     (vec int v))))
         '(f v))
  (parse '() '(vec int v))
  )
