#lang racket

(require eopl/datatype
         "../base/utils.rkt"
         "../cps/builtin.rkt"
         "../types/type.rkt")

(provide (all-defined-out))

(define-datatype
  definition defn?
  (define-defn
    (var symbol?)
    (val expression?))
  (type-defn
    (tv symbol?)
    (t Type?))
  (mod-defn
    (mn symbol?)
    (decls (list-of decl?))
    (defns (list-of defn?)))
  )

(define-datatype
  declaration decl?
  (opaque-type-decl
    (t symbol?))
  (val-decl
    (v symbol?)
    (t Type?))
  (transparent-type-decl
    (v symbol?)
    (t Type?)))

(define-datatype
  expression expression?
  (const-exp
    (cst const?))
  (var-exp
    (var symbol?))
  (quote-exp
    (sexp sexp?))
  (op-exp
    (op op?)
    (params (list-of expression?)))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  (if-exp
    (test expression?)
    (then expression?)
    (else expression?))
  (lambda-exp
    (vars (list-of symbol?))
    (body expression?))
  (compound-exp
    (exps (list-of expression?)))
  (letrec-exp
    (p-names (list-of symbol?))
    (procs (list-of expression?))
    (body expression?))
  (set-exp
    (var symbol?)
    (val expression?))
  )

(define (single-or-compound exps)
  (if (null? (cdr exps))
    (car exps)
    `(begin
       ,@exps)))

(define (parse-t t)
  (match t
    ['int (Num)]
    ['str (Str)]
    ['bool (Bool)]
    ['atom (Atom)]
    ['void (Unit)]
    [(? symbol?)
     (T-Var t)]
    [(list) (Nil)]
    [(list vts '-> t)
     (Fun (map (lambda (t)
                 (parse-t t))
               vts)
          (parse-t t))]
    [`(mod ,decls ...)
      (let ((decls (map parse-decl decls)))
        (match (expand-decls decls)
          [(list vs ts)
           (Mod vs ts)]))]
    [_ (error 'parse-t "match ~a failed" t)]
    ))

(define (gen-decls vs ts)
  (map (lambda (v t)
         (gen-decl v t))
       vs
       ts))

(define (gen-decl v t)
  (match v
    [(T-Var v)
     (match t
       [(Opaque t)
        (opaque-type-decl t)]
       [_ (transparent-type-decl v t)])]
    [(V-Var v)
     (val-decl v t)]))

(define (unparse-t t)
  (match t
    [(Num) 'int]
    [(Str) 'str]
    [(Bool) 'bool]
    [(Atom) 'atom]
    [(Unit) 'void]
    [(Nil) '()]
    [(Var v)
     ;(error 'unparse-t "unparse Var")]
     v]
    [(Opaque v)
     ;(error 'unparse-t "unparse opaque")]
     v]
    [(T-Var v)
     v]
    [(Pair a d)
     (cons (unparse-t a)
           (unparse-t d))]
    [(Fun vts rt)
     `(,(map unparse-t vts) -> ,(unparse-t rt))]
    [(Mod vars types)
     (let ((decls (map unparse-decl (gen-decls vars types))))
       `(mod ,@decls))]
    [_ (error 'unparse-t "match ~a failed" t)]
    ))


(define (print-vts vs ts)
  (for-each (lambda (v t)
              (let ((v (match v
                         [(T-Var v) v]
                         [(V-Var v) v])))
                (printf "~a : ~a\n" v (unparse-t t))))
            vs
            ts))

(define (expand-decls decls)
  (let loop ((decls decls)
             (vs '())
             (ts '()))
    (if (null? decls)
      (list (reverse vs)
            (reverse ts))
      (match (expand-decl (car decls))
        [(list v t)
         (loop (cdr decls)
               (cons v vs)
               (cons t ts))]))))

(define (expand-decl decl)
  (cases declaration decl
    (opaque-type-decl
      (t)
      (list (T-Var t) (gen-opaque-type t)))
    (val-decl
      (v t)
      ; t is already parsed
      (list (V-Var v) t))
    (transparent-type-decl
      (v t)
      ; t is already parsed
      (list (T-Var v) t))))

(define (parse-decl decl)
  (match decl
    [`(type ,v)
      (opaque-type-decl v)]
    [`(val ,v ,t)
      (val-decl v (parse-t t))]
    [`(type ,v ,t)
      (transparent-type-decl v
                             (parse-t t))]
    [_ (error 'parse-decl "match ~a failed" decl)]
    ))

(define (unparse-decl decl)
  (cases declaration decl
    (opaque-type-decl
      (v)
      `(type ,v))
    (val-decl
      (v t)
      `(val ,v ,(unparse-t t)))
    (transparent-type-decl
      (v t)
      `(type ,v ,(unparse-t t)))
    ))
    
(define (parse-defn defn)
  (match defn
    [`(define ,var ,val)
      (define-defn var (parse-exp val))]
    [`(type ,v)
      (error 'parse-defn "opaque type defn is not allowed")]
    [`(type ,tv ,t)
      (type-defn tv
                 (parse-t t))]
    [`(module ,mname
        (sig ,decls ...)
        (body ,defns ...))
      (mod-defn mname
                (map parse-decl decls)
                (map parse-defn defns))]))

(define (parse-exp sexp)
  (match sexp
    [(? const? x) (const-exp x)]
    [(? symbol? x) (var-exp x)]
    ; symbol
    [`(quote ,x) (quote-exp x)]
    ; builtin ops
    [(list (? op? op) params* ...)
     (op-exp op (map parse-exp params*))]
    ; if 
    [`(if ,test ,then ,else)
      (if-exp (parse-exp test)
              (parse-exp then)
              (parse-exp else))]
    ; lambda
    [`(lambda (,params ...) ,body ,bodies* ...)
      (lambda-exp params
                  (parse-exp (single-or-compound (cons body bodies*))))]
    [`(begin ,body ,bodies* ...)
      (compound-exp (map parse-exp (cons body bodies*)))]
    [`(let ((,var ,val) ...) ,body ,bodies* ...)
      (parse-exp `((lambda (,@var)
                 ,@(cons body bodies*))
               ,@val))]
    [`(letrec ((,name* ,proc*) ...) ,body ,bodies* ...)
      (letrec-exp name*
                  (map parse-exp proc*)
                  (parse-exp (single-or-compound (cons body bodies*))))]
    [`(set! ,var ,val)
      (set-exp var
               (parse-exp val))]
    [`(cond (,pred* ,body*) ...)
      (if (not (null? pred*))
        (let ((pred (car pred*))
              (body (car body*)))
          (cond ((and (eq? pred 'else)
                      (null? (cdr pred*)))
                 (parse-exp body))
                ((eq? pred 'else)
                 (error 'parse-exp "cond else should be the last expression"))
                (else
                  (parse-exp `(if ,pred
                            ,body
                            (cond ,@(map (lambda (pred body)
                                           (list pred body))
                                         (cdr pred*)
                                         (cdr body*))))))))
        '(void))
      ]
    ; procedure call
    [(list rand rators ...)
     (call-exp (parse-exp rand)
               (map (lambda (rator)
                      (parse-exp rator))
                    rators))]
    ))
