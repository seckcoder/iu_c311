#lang racket
; type check
(require eopl/datatype
         "parser.rkt"
         "../types/store.rkt"
         "../base/utils.rkt"
         "../types/type.rkt"
         "constraint.rkt"
         "type-check.rkt"
         (prefix-in Env. "type-env.rkt"))

(provide typeof
         typeof-t)

(struct TypeRet (v env subst))
(struct DeclRet (v env) #:transparent)
(struct DefnRet (v env subst) #:transparent)

; for multi args
(define (typeof-multi-exps exps env subst)
  (let loop ((rev-exp-types '())
             (subst subst)
             (exps exps))
    (if (null? exps)
      (list (reverse rev-exp-types) subst)
      (match (typeof-exp (car exps) env subst)
        [(TypeRet exp-type _ subst)
         (loop (cons exp-type rev-exp-types) subst (cdr exps))]))))

; for compound exps
(define (typeof-compound-exps exps env subst)
  (foldl
    (lambda (exp acc)
      (match acc
        [(TypeRet t env subst)
         (match (typeof-exp exp env subst)
           [(TypeRet t env subst)
            (TypeRet t env subst)])]))
    (TypeRet 'void env subst)
    exps))

(define (typeof-defns defns env subst)
  (foldl
    (lambda (defn acc)
      (match acc
        [(DefnRet vs env subst)
         (match (typeof-defn defn env subst)
           [(DefnRet v env subst)
            (DefnRet (append vs (list v))
                     env
                     subst)])]))
    (DefnRet '() env subst)
    defns))

(define (typeof-decls decls env)
  (foldl
    (lambda (decl acc)
      (match acc
        [(DeclRet vs env)
         (match (typeof-decl decl env)
           [(DeclRet v env)
            (DeclRet (append vs (list v))
                     env)])]))
    (DeclRet '() env)
    decls))


(define (typeof-defn defn env subst)
  (cases definition defn
    (define-defn
      (var val-exp)
      (match (typeof-exp val-exp env subst)
        [(TypeRet t _ subst)
         (let ((vv (V-Var var)))
           (DefnRet vv
                    (Env.extend vv t env)
                    subst))]))
    (type-defn
      (tv t)
      (let ((tv (T-Var tv))
            (t (typeof-t t env)))
        (DefnRet tv
                 (Env.extend tv t env)
                 subst)))
    (mod-defn
      (mn decls defns)
      (match (typeof-decls decls env)
        [(DeclRet decl-vs decl-env)
         (match (typeof-defns defns env subst)
           [(DefnRet defn-vs defn-env subst)
            (match
              (type-check-mod
                decl-vs (map (lambda (v)
                               (Env.apply decl-env v))
                             decl-vs)
                defn-vs (map (lambda (v)
                               (Env.apply defn-env v))
                             defn-vs)
                env subst exp)
              [(list mod subst)
               (let ((tv (V-Var mn)))
                 (DefnRet tv
                          (Env.extend tv mod env)
                          subst))])])]))
    ))

(define (typeof-decl decl env)
  (cases declaration decl
    (opaque-type-decl
      (t)
      (let ((tv (T-Var t)))
        (DeclRet tv
                 (Env.extend tv (gen-opaque-type t) env))))
    (val-decl
      (v t)
      (let ((vv (V-Var v))
            (t (typeof-t t env)))
        (DeclRet vv
                 (Env.extend vv t env))))
    (transparent-type-decl
      (v t)
      (let ((tv (T-Var v))
            (t (typeof-t t env)))
        (DeclRet tv
                 (Env.extend tv t env))))
    ))

(define (typeof-exp exp env subst)
  (cases expression exp
    (const-exp
      (cst)
      (let ((t (match cst
                 [(? number?) (Num)]
                 [(? string?) (Str)]
                 [(? boolean?) (Bool)]
                 [(? null?) (Nil)]
                 [(? void?) (Unit)]
                 )))
        (TypeRet t env subst)))
    (var-exp
      (var)
      (TypeRet (Env.apply env (V-Var var))
               env
               subst))
    (quote-exp
      (sexp)
      (letrec ((typeof-sexp (lambda (sexp)
                              (match sexp
                                [(? atom?) (Atom)]
                                [(cons a d)
                                 (Pair (typeof-sexp a)
                                       (typeof-sexp d))]))))
        (TypeRet (typeof-sexp sexp)
                 env
                 subst)))
    (op-exp
      (op rands)
      (let ((typeof-op (lambda (op-rand-types op-ret-type)
                         (TypeRet op-ret-type
                                  env
                                  (match (typeof-multi-exps rands env subst)
                                    [(list cur-rand-types subst)
                                     (unify-multi subst
                                                  cur-rand-types
                                                  op-rand-types
                                                  exp)]))))
            (rand-num (length rands)))
        (cond ((memq op '(+ - * / =))
               (typeof-op (v->lst rand-num (Num)) (Num)))
              ((memq op '(zero?))
               (typeof-op (list (Num)) (Bool)))
              ; list support; eopl 7.25
              ((eq? op 'list)
               (match (typeof-multi-exps rands env subst)
                 [(list rand-types subst)
                  (TypeRet (lst-of-t->pair rand-types)
                           env
                           subst)]))
              ((eq? op 'car)
               (if (not (= rand-num 1))
                 (error 'car "arity mismatch")
                 (match (typeof-exp (car rands) env subst)
                   [(TypeRet rand-t _ subst)
                    (TypeRet (Pair-a rand-t)
                             env
                             subst)])))
              ((eq? op 'cdr)
               (if (not (= rand-num 1))
                 (error 'cdr "arity mismatch")
                 (match (typeof-exp (car rands) env subst)
                   [(TypeRet rand-t _ subst)
                    (TypeRet (Pair-d rand-t)
                             env
                             subst)])))
              ((eq? op 'cons)
               (if (not (= rand-num 2))
                 (error 'cons "arity mismatch")
                 (match (typeof-multi-exps rands env subst)
                   [(list rand-types subst)
                    (TypeRet (Pair (car rand-types)
                                   (cadr rand-types))
                             env
                             subst)])))
              (else
                (error 'typeof-exp "op:~s not supported" op)))))
    (lambda-exp
      (params body)
      (let* ((param-tvars (map (lambda (v) (gen-type-var)) params))
             (new-env (Env.extends* (map V-Var params) param-tvars env)))
        (match (typeof-exp body new-env subst)
          [(TypeRet body-type _ new-subst)
           (TypeRet (Fun param-tvars
                         body-type)
                    env
                    new-subst)])))
    (if-exp
      (test then else)
      (match (typeof-exp test env subst)
        [(TypeRet test-type _ subst)
         (match (typeof-exp then env (unify subst test-type 'bool exp))
           [(TypeRet then-type _ subst)
            (match (typeof-exp else env subst)
              [(TypeRet else-type _ subst)
               (TypeRet else-type
                        env
                        (unify subst then-type else-type exp))])])]))
    (letrec-exp
      (p-names procs body)
      (let* ((p-typevars (map (lambda (_) (gen-type-var)) p-names))
             (new-env (Env.extends** (map V-Var p-names)
                                     p-typevars env)))
        (match (typeof-multi-exps procs new-env subst)
          [(list p-types subst)
           (match
             (typeof-exp body
                           new-env
                           (unify-multi subst p-typevars p-types exp)
                           )
             [(TypeRet body-type _ subst)
              (TypeRet body-type env subst)])])))
    (compound-exp
      (exps)
      (typeof-compound-exps exps env subst))
    (set-exp
      (var val)
      (match (typeof-exp val env subst)
        [(TypeRet val-type _ subst)
         (let* ((var-type (Env.apply env (V-Var var)))
                (subst (unify subst var-type val-type exp)))
           (TypeRet (Unit) env subst))]))
    (call-exp
      (rator rands)
      (match (typeof-exp rator env subst)
        [(TypeRet rator-type _ subst)
         (let ((exp-tvar (gen-type-var)))
           (TypeRet exp-tvar
                    env
                    (match (typeof-multi-exps rands env subst)
                      [(list rand-types subst)
                       (unify subst rator-type (Fun rand-types exp-tvar) exp)])))]))
    ))

(define (typeof defns exp)
  (initialize-store!)
  (match (typeof-defns (map parse-defn defns) (Env.empty) '())
    [(DefnRet _ env subst)
     (match (typeof-exp (parse-exp exp) env subst)
       [(TypeRet type env subst)
        (apply-subst-to-type subst type)])]))

; ============TEST====================


(module+ test-
  ; test typeof-exp
  (require rackunit)
  (define test-typeof-exp
    (match-lambda*
      [(list exp)
       (initialize-store!)
       (match (typeof-exp (parse-exp exp)
                          (Env.empty)
                         '())
        [(TypeRet t _ subst)
         (unparse-t (apply-subst-to-type subst t))])]
      [(list exp type)
       (check equal?
              (test-typeof-exp exp)
              type)]
      ))
  (test-typeof-exp '3 'int)
  (test-typeof-exp '(lambda (v) v))
  (test-typeof-exp '(lambda (v)
                      (zero? v))
                   '((int) -> bool))
  )

(module+ test
  ; compound test
  (define (test-typeof-defn defn)
    (initialize-store!)
    (match
      (typeof-defn (parse-defn defn)
                   (Env.empty)
                   '())
      [(DefnRet v env subst)
       (printf "~a\n~a\n" (unparse-t (Env.apply env v))
               subst)]))
  ;(test-typeof-defn '(define v 3))
  (test-typeof-defn '(module m
                       (sig
                         (type t)
                         (type t1 int)
                         (val u t)
                         )
                       (body
                         (type t int)
                         (type t1 int)
                         (define u 3)
                         )))
  )
