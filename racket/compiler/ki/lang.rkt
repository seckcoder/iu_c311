#lang racket

(require "../../base/utils.rkt")

(define (const? x)
  (or (number? x)
      (string? x)
      (boolean? x)))

(struct Program (main clses) #:transparent)
(struct Cls (name base decls) #:transparent)
(struct Def (v t val) #:transparent)
(struct DefStatic (v t val) #:transparent)
(struct Fun (v body) #:transparent)
(struct Vec (vs) #:transparent)
(struct VecRef (v i) #:transparent)
(struct VecSet (v i val) #:transparent)
(struct Block (clauses)  #:transparent)
(struct New (c vs) #:transparent)
(struct Const (v) #:transparent)
(struct Var (v) #:transparent)
(struct If-s (test then else) #:transparent)
(struct Set-s (v val) #:transparent)
(struct Return (val) #:transparent)
(struct Biop (op a b) #:transparent)
(struct Unop (op v) #:transparent)
(struct App (rator rands) #:transparent)

(struct Type () #:transparent)
(struct Int Type () #:transparent)
(struct Str Type () #:transparent)
(struct Bool Type () #:transparent)
(struct VecType Type (t) #:transparent)
(struct FunType Type (rand ret) #:transparent)
(struct VoidType Type () #:transparent)

(define parse-t
  (match-lambda
    ['int (Int)]
    ['str (Str)]
    ['bool (Bool)]
    ['void (VoidType)]
    [`(vec ,t) (VecType (parse-t t))]
    [`(,t1 -> ,t2)
      (FunType
        (parse-t t1)
        (parse-t t2))]
    [`(,t1 -> ,t2 ...)
      (FunType
        (parse-t t1)
        (parse-t t2))]))


(define parse-decl
  (match-lambda
    [(list `(: ,t)
           `(def ,v))
     (Def v (parse-t t) (void))]
    [(list `(: ,t)
           `(def ,v ,val))
     (Def v (parse-t t) (parse-exp val))]
    [(list `(: ,t)
           `(def-static ,v))
     (DefStatic v (parse-t t) (void))]
    [(list `(: ,t)
           `(def-static ,v ,val))
     (DefStatic v (parse-t t) (parse-exp val))]
    ))

(define parse-cls
  (match-lambda
    [`(class (,name ,base) ,decls ...)
      (Cls name base
           (map parse-decl (group decls 2)))]
    [`(class ,name ,decls ...)
      (parse-cls `(class (,name Object) ,@decls))]))

; statement
(define parse-stms
  (match-lambda
    ; if statement
    [`(if ,test ,then ,else)
      (If-s (parse-exp test)
            (parse-stms then)
            (parse-stms else))]
    ; set statement
    [`(set! ,v ,val)
      (Set-s v (parse-exp val))]
    [`(vec-set! ,v ,i ,val)
      (VecSet (parse-exp v)
              (parse-exp i)
              (parse-exp val))]
    [`(begin ,clause0 ,clauses* ...)
      (Block (parse-body
               (cons clause0
                     clauses*)))]
    [`(return ,val)
      (Return (parse-exp val))]
    [`(,rator ,rand* ...)
      (App rator rand*)]
    ))

(define (bi-op? op)
  (memq op '(+ - * and or = < > <= >= )))
(define (un-op? op)
  (memq op '(not)))

(define parse-exp
  (match-lambda
    [(? const? x) (Const x)]
    [(? symbol? x) (Var x)]
    ; bi-op: + - * = and or
    ; un-op: not
    [(list (? bi-op? op) a b)
     (Biop op a b)]
    [(list (? un-op? op) v)
     (Unop op v)]
    ; fn exp
    [`(fn () ,body ...)
      (Fun (void)
           (parse-body body))]
    ; fn exp
    [`(fn (,v) ,body ...)
      (Fun v (parse-body body))]
    ; fn exp
    [`(fn (,v0 ,v* ...) ,body ...)
      (Fun v0
           (parse-exp `(fn ,v* ,@body)))]
    ; vec exp
    [`(vec ,v* ...)
      (Vec (map parse-exp v*))]
    [`(vec-ref ,v ,i)
      (VecRef (parse-exp v)
              (parse-exp i))]
    [`(new ,cls ,v* ...)
      (New cls (map parse-exp v*))]
    ))


(define (is-decl? decl)
  (and (pair? decl)
       (eq? (car decl) ':)))

; clause: statement | declaration
(define parse-clause
  (match-lambda
    [(? is-decl? decl)
     (parse-decl decl)]
    [stms
     (parse-stms stms)]
    ))

; fn body
(define (parse-body clauses)
  (groupf
    clauses
    [(compose is-decl? car)
     (lambda (clauses)
       (values (parse-decl (take clauses 2))
               (drop clauses 2)))]
    [(compose not is-decl? car)
     (lambda (clauses)
       (values (parse-stms (car clauses))
               (cdr clauses)))]
    ))

(define parse
  (lambda (main-c clses)
    (Program (parse-cls main-c)
             (map parse-cls clses))))

(module+ test
  (parse-exp '(fn (a b)
                  (return (vec a b))))
  (parse-cls '(class Cls
                (: int)
                (def v)
                (: int)
                (def v2 (vec 3))
                (: (int -> int -> (vec int)))
                (def f (fn (x1 x2)
                         (return (vec x1 x2))))))
  (parse '(class Fac
            (: (int -> int))
            (def ComputeFac
                 (fn (num)
                   (: int)
                   (def num_aux)
                   (if (< num 1)
                     (set num_aux 1)
                     (set num_aux
                          (* num
                             (.ComputeFac this (- num 1)))))
                   (return num_aux))))
         (list
           '(class Factorial
              (: ((vec str) -> void))
              (def-static main
                          (fn (a)
                            (printf
                              (.ComputerFac (new Fac) 10)))
                          ))))
  (parse-stms '(begin
                 (: int)
                 (def v 3)
                 (set! v 4)))
  )
