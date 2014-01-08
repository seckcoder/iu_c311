#lang racket
; type check
(require eopl/datatype
         "parser.rkt"
         "../base/utils.rkt"
         "../types/type.rkt"
         "../types/store.rkt"
         "env.rkt")

(provide typeof)

; for multi args
(define (typeof-multi/subst exps env subst)
  (let loop ((rev-exp-types '())
             (subst subst)
             (exps exps))
    (if (null? exps)
      (list (reverse rev-exp-types) subst)
      (match (typeof/subst (car exps) env subst)
        [(TypeRet exp-type _ _ subst)
         (loop (cons exp-type rev-exp-types) subst (cdr exps))]))))

; for compound exps
(define (typeof-compound/subst exps env subst)
  (foldl
    (lambda (exp acc)
      (match acc
        [(TypeRet t vars env subst)
         (match (typeof/subst exp env subst)
           [(TypeRet t vars1 env subst)
            (TypeRet t (append vars vars1) env subst)])]))
    (TypeRet 'void '() env subst)
    exps))

(define (unify-multi subst types1 types2 exp)
  #|(println types1)(println types2)
  (println (length types1)) (println (length types2))|#
  (foldl (lambda (ty1 ty2 subst)
           (unify subst ty1 ty2 exp))
         subst
         types1
         types2))

(define (equa left right)
  (list left right))

; unify ty1=ty2 to substitution
(define (unify subst ty1 ty2 exp)
  (let loop ((ty1 (apply-subst-to-type subst ty1))
             (ty2 (apply-subst-to-type subst ty2)))
    (if (equal? ty1 ty2)
      ; useless equation
      subst
      (match ty1
        [(? simpletype?)
         (match ty2
           [(or (? simpletype?)
                (? Fun?)
                (? Mod?)
                (? Pair?))
            (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
           [(? Var?)
            (loop ty2 ty1)])]
        [(? Var?)
         (if (occurs? ty1 ty2)
           (error 'unify "fail occurrence check for expression:~s" exp)
           (extend-subst subst ty1 ty2))]
        [(Fun ty1-vts ty1-rt)
         (match ty2
           [(or (? simpletype?)
                (? Pair?)
                (? Mod?))
            (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
           [(? Var?)
            (loop ty2 ty1)]
           [(Fun ty2-vts ty2-rt)
            (unify-multi subst
                         (cons ty1-rt ty1-vts)
                         (cons ty2-rt ty2-vts)
                         exp)])]
        [(Pair a1 d1)
         (match ty2
           [(or (? simpletype?)
                (? Fun?)
                (? Mod?))
            (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
           [(? Var?)
            (loop ty2 ty1)]
           [(Pair a2 d2)
            (unify-multi subst
                         (list a1 d1)
                         (list a2 d2)
                         exp)]
           )]
        [(? Mod?)
         (match ty2
           [(or (? simpletype?)
                (? Pair?)
                (? Fun?))
            (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
           [(? Var?)
            (loop ty2 ty1)]
           [(? Mod?)
            (unify-mod subst ty1 ty2 exp)])]
        ))))

; Que:
; 1. How to unify the signature and the body when an transparent type or opague
; type in signature corresponds to an opague type?
; 2. The order of ty1 and ty2 is important here(ty1 :< ty2). Any method to solve this?
(define (unify-mod subst ty1 ty2 exp)
  (match ty1
    [(Mod vars1 types1)
     (match ty2
       [(Mod vars2 types2)
        (let ((type-pairs (map list vars2 types2)))
          (foldl
            (lambda (var1 type1 subst)
              (match (assq var1 type-pairs)
                [#f
                 (error 'typeof "interface declaration ~a = ~a not exist in module:~s"
                        var1 type1 exp)]
                [(list _ type2)
                 (unify subst
                        type1
                        type2
                        exp)]))
            subst
            vars1
            types1))])]))

; substitution of a type for a type variable. type[sym = new-type]
(define replace
  (lambda (type tvar new-type)
    ; replace every sym in type with new-type
    (match type
      [(? Var?)
       (if (equal? tvar type)
         new-type
         type)]
      [(Mod vars types)
       (Mod vars (map (lambda (t)
                        (replace t tvar new-type))
                      types))]
      [(Pair a d)
       (Pair (replace a tvar new-type)
             (replace d tvar new-type))]
      [(Fun vts rt)
       (Fun (map (lambda (sub-type)
                   (replace sub-type tvar new-type))
                 vts)
            (replace rt tvar new-type))]
      [_ type]
      )))

; subst[ty1=ty2] replace subst's bindings with ty2
(define (extend-subst subst ty1 ty2)
  (cons (equa ty1 ty2)
        (map (lambda (subst-equa)
               (match subst-equa
                 [(list tyvar tybind)
                  (equa tyvar (replace tybind ty1 ty2))]))
             subst)))

; occurs check
(define (occurs? tvar type)
  (match type
    [(? Var?) (equal? tvar type)]
    ; check for pair
    [(Pair a d)
     (or (occurs? tvar a)
         (occurs? tvar d))]
    [(Fun vts rt)
     (ormap (lambda (t)
              (occurs? tvar t))
            (cons rt vts))]
    [_ #f]
    ))

; apply subst to type(replace type with bindings)
; or lookup type in subst
(define (apply-subst-to-type subst type)
  (if (no-var? type)
    type  ; a tiny optimization. only apply for type has variable(eopl 7.20)
    (foldl (lambda (subst-equa type)
             (match subst-equa
               [(list tyvar tybind)
                (replace type tyvar tybind)]))
           type
           subst)))

(struct TypeRet (tvar vars env subst))

; exp * env * subst -> type * (var list) * env subst
; (var list): The list of new vars introduced by the exp. Only `define`
;             and `module` will introduce new var
; env : new env extended from the input env (the extended vars are (var list))
; subst : new substitution
(define (typeof/subst exp env subst)
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
        (TypeRet t '() env subst)))
    (var-exp
      (var)
      (TypeRet (deref (apply-env env var))
               '()
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
                 '()
                 env
                 subst)))
    (op-exp
      (op rands)
      (let ((typeof-op (lambda (op-rand-types op-ret-type)
                         (TypeRet op-ret-type
                                  '()
                                  env
                                  (match (typeof-multi/subst rands env subst)
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
               (match (typeof-multi/subst rands env subst)
                 [(list rand-types subst)
                  (TypeRet (lst-of-t->pair rand-types)
                           '()
                           env
                           subst)]))
              ((eq? op 'car)
               (if (not (= rand-num 1))
                 (error 'car "arity mismatch")
                 (match (typeof/subst (car rands) env subst)
                   [(TypeRet rand-t _ _ subst)
                    (TypeRet (Pair-a rand-t)
                             '()
                             env
                             subst)])))
              ((eq? op 'cdr)
               (if (not (= rand-num 1))
                 (error 'cdr "arity mismatch")
                 (match (typeof/subst (car rands) env subst)
                   [(TypeRet rand-t _ _ subst)
                    (TypeRet (Pair-d rand-t)
                             '()
                             env
                             subst)])))
              ((eq? op 'cons)
               (if (not (= rand-num 2))
                 (error 'cons "arity mismatch")
                 (match (typeof-multi/subst rands env subst)
                   [(list rand-types subst)
                    (TypeRet (Pair (car rand-types)
                                   (cadr rand-types))
                             '()
                             env
                             subst)])))
              (else
                (error 'typeof/subst "op:~s not supported" op)))))
    (lambda-exp
      (params body)
      (let* ((param-tvars (map (lambda (v) (gen-type-var)) params))
             (new-env (extend-envs params (newrefs param-tvars) env)))
        (match (typeof/subst body new-env subst)
          [(TypeRet body-type _ _ new-subst)
           (TypeRet (Fun param-tvars
                         body-type)
                    '()
                    env
                    new-subst)])))
    (if-exp
      (test then else)
      (match (typeof/subst test env subst)
        [(TypeRet test-type _ _ subst)
         (match (typeof/subst then env (unify subst test-type 'bool exp))
           [(TypeRet then-type _ _ subst)
            (match (typeof/subst else env subst)
              [(TypeRet else-type _ _ subst)
               (TypeRet else-type
                        '()
                        env
                        (unify subst then-type else-type exp))])])]))
    (letrec-exp
      (p-names procs body)
      (let* ((p-typevars (map (lambda (_) (gen-type-var)) p-names))
             (new-env (extend-envs p-names (newrefs p-typevars) env)))
        (match (typeof-multi/subst procs new-env subst)
          [(list p-types subst)
           (match
             (typeof/subst body
                           new-env
                           (unify-multi subst p-typevars p-types exp)
                           )
             [(TypeRet body-type _ _ subst)
              (TypeRet body-type '() env subst)])])))
    (compound-exp
      (exps)
      (typeof-compound/subst exps env subst))
    (set-exp
      (var val)
      (match (typeof/subst val env subst)
        [(TypeRet val-type _ _ subst)
         (let* ((var-type (deref (apply-env env var)))
                (subst (unify subst var-type val-type exp)))
           (TypeRet (Unit) '() env subst))]))
    (define-exp
      (var val)
      (let* ((tvar (gen-type-var))
             (new-env (extend-env var (newref tvar) env)))
        (match (typeof/subst val new-env subst)
          [(TypeRet val-type _ _ subst)
           (TypeRet (Unit)
                    (list var)
                    new-env
                    (unify subst tvar val-type exp))])))
    (module-exp
      (mname sigs bodies)
      (match (typeof-compound/subst sigs env subst)
        [(TypeRet _ sig-vars sig-env subst)
         (match (typeof-compound/subst bodies env subst)
           [(TypeRet _ body-vars body-env subst)
            (let* ((sig-types (apply-envs sig-env sig-vars deref))
                   (mod-sig-type (Mod sig-vars sig-types))
                   (body-types (apply-envs body-env body-vars deref))
                   (mod-body-type (Mod body-vars body-types))
                   (subst (unify-mod subst mod-sig-type mod-body-type exp))
                   (mod-type-var (gen-type-var)))
              (TypeRet mod-type-var
                       (list mname)
                       (extend-env mname
                                   (newref mod-type-var)
                                   env)
                       (unify subst
                              mod-type-var
                              mod-sig-type
                              exp)))])]))
    (import-exp
      (mname)
      (TypeRet (Unit)
               '()
               (import-mod mname env subst)
               subst))
    (def-transparent-type-exp
      (v t)
      (let ((vt (sym->type t
                           (lambda (v)
                             (deref (apply-env env v))))))
        (TypeRet (Unit)
                 (list v)
                 (extend-env v
                             (newref vt)
                             env)
                 subst)))
    (def-opague-type-exp
      (v)
      (let ((tvar (gen-type-var)))
        (TypeRet (Unit)
                 (list v)
                 (extend-env v (newref tvar) env)
                 subst)))
    (call-exp
      (rator rands)
      (match (typeof/subst rator env subst)
        [(TypeRet rator-type _ _ subst)
         (let ((exp-tvar (gen-type-var)))
           (TypeRet exp-tvar
                    '()
                    env
                    (match (typeof-multi/subst rands env subst)
                      [(list rand-types subst)
                       (unify subst rator-type (Fun rand-types exp-tvar) exp)])))]))
    ))

(define (import-mod mname env subst)
  (match (apply-subst-to-type subst (deref (apply-env env mname)))
    [(Mod vars types)
     (extend-envs (map (lambda (var)
                         (sym-append mname ': var))
                       vars)
                  (newrefs types)
                  env)]))

(define (typeof exp)
  (initialize-store!)
  (match (typeof/subst (parse exp) (empty-env) '())
    [(TypeRet type _ _ subst)
     (apply-subst-to-type subst type)]))

(module+ test
  (require rackunit)
  (define (test-typeof exp)
    (type->sym (typeof exp)))
  ;(test-typeof '(lambda (v) v))
  #|(test-typeof '(lambda (v)
                    (zero? v)))|#

  ;(test-typeof '(lambda (f) (f 1)))
  ;(test-typeof '(define v 3))
  #|(test-typeof '(begin
                    (define v 3)
                    (lambda (v)
                      v)))|#
  #|(test-typeof '(module m1
                    (sig
                      (deftype u int)
                      (deftype f ((int) -> int)))
                    (body
                      (define u 3)
                      (define f (lambda (v) v)))))|#
  #|(test-typeof '(module m1
                    (sig
                      (u int))
                    (body
                      (define u 3)
                      (define v 5))))|#
  (test-typeof '(begin
                    (module m1
                      (sig
                        (deftype u int)
                        (deftype m2 (mod (v f) (int ((int) -> int))))
                        (deftype v int))
                      (body
                        (define u 3)
                        (module m2
                          (sig
                            (deftype f ((int) -> int))
                            (deftype v int))
                          (body
                            (define v 4)
                            (define f (lambda (v) (+ v 3)))
                            ))
                        (import m2)
                        (define v m2:v)
                        ))
                    (import m1)
                    (import m1:m2)
                    m1:m2:v
                    ))
  ;(test-typeof '(list 1 2 3))
  #|(test-typeof '(lambda (l)
                    (cons 3 l)))|#
  (test-typeof '(module m
                    (sig
                      (deftype p (int . int))
                      (deftype p1 (int . bool)))
                    (body
                      (define p (cons 1 2))
                      (define p1 (cons 1 #f))
                      )))
  (test-typeof '(begin
                  (module m
                    (sig
                      (deftype t1 int) ; transparent
                      (deftype t2 int) ; opague
                      (deftype u t1)
                      (deftype f ((t1) -> t1))
                      (deftype g ((t2) -> t2))
                      )
                    (body
                      (deftype t1 int)
                      (deftype t2)
                      (define u 3)
                      (define f (lambda (v)
                                  v))
                      (define g (lambda (v)
                                  v))))
                  (import m)
                  m:g
                  ))
  )
