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
        [(list exp-type _ _ subst)
         (loop (cons exp-type rev-exp-types) subst (cdr exps))]))))

; for compound exps
(define (typeof-compound/subst exps env subst)
  (foldl
    (lambda (exp acc)
      (match acc
        [(list t vars env subst)
         (match (typeof/subst exp env subst)
           [(list t vars1 env subst)
            (list t (append vars vars1) env subst)])]))
    (list 'void '() env subst)
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
    (cond ((equal? ty1 ty2)
           ; useless equation
           subst)
          ((simpletype? ty1)
           (match ty2
             [(or (? simpletype? ty2)
                  (? proctype? ty2)
                  (? modtype? ty2)
                  )
              (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
             [(? typevar? ty2)
              (loop ty2 ty1)]))
          ((typevar? ty1)
           (if (occurs? ty1 ty2)
             (error 'unify "fail occurrence check for expression:~s" exp)
             (extend-subst subst ty1 ty2)))
          ((proctype? ty1)
           (match ty2
             [(or (? simpletype? ty2)
                  (? modtype? ty2))
              (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
             [(? typevar? ty2)
              (loop ty2 ty1)]
             [(? proctype? ty2)
              (let ((ty1-vars (proctype-vars ty1))
                    (ty2-vars (proctype-vars ty2))
                    (ty1-ret (proctype-ret ty1))
                    (ty2-ret (proctype-ret ty2)))
                (unify-multi subst
                             (cons ty1-ret ty1-vars)
                             (cons ty2-ret ty2-vars)
                             exp))]))
          ((modtype? ty1)
           (match ty2
             [(or (? simpletype? ty2)
                  (? proctype? ty2))
              (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
             [(or (? typevar? ty2))
              (loop ty2 ty1)]
             [(or (? modtype? ty2))
              (unify-mod subst ty1 ty2 exp)]))
          )))


; We assume ty1 <: ty2
(define (unify-mod subst ty1 ty2 exp)
  (match ty1
    [`(mod type ,vars1 ,types1)
      (match ty2
        [`(mod type ,vars2 ,types2)
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
  (lambda (type sym new-type)
    ; replace every sym in type with new-type
    (match type
      [(? symbol? t)
       (if (eq? sym t)
         new-type
         t)]
      [(? modtype? t)
       ; we can't replace module type
       t]
      [(list (list var-types ...) ret-type)
       (proctype (map (lambda (sub-type)
                        (replace sub-type sym new-type))
                      var-types)
                 (replace ret-type sym new-type))])))

; subst[ty1=ty2] replace subst's bindings with ty2
(define (extend-subst subst ty1 ty2)
  (cons (equa ty1 ty2)
        (map (lambda (subst-equa)
               (match subst-equa
                 [(list tyvar tybind)
                  (equa tyvar (replace tybind ty1 ty2))]))
             subst)))

; occurs check
(define (occurs? sym type)
  (match type
    [(? symbol? t) (eq? sym t)]
    [(? modtype? type) #f]
    [(list (list var-types ...) ret-type)
     (ormap (lambda (t)
              (occurs? sym t))
            (cons ret-type var-types))]))

; apply subst to type(replace type with bindings)
; or lookup type in subst
(define (apply-subst-to-type subst type)
  (if (simpletype? type)
    type  ; a tiny optimization. only apply for non simpletype(eopl 7.20)
    (foldl (lambda (subst-equa type)
             (match subst-equa
               [(list tyvar tybind)
                (replace type tyvar tybind)]))
           type
           subst)))

; exp * env * subst -> type * (var list) * env subst
; (var list): The list of new vars introduced by the exp. Only `define`
;             and `module` will introduce new var
; env : new env extended from the input env (the extended vars are (var list))
; subst : new substitution
(define (typeof/subst exp env subst)
  (cases expression exp
    (const-exp
      (cst)
      (list (typeof-const cst)
            '()
            env
            subst))
    (var-exp
      (var)
      (list (deref (apply-env env var))
            '()
            env
            subst))
    (quote-exp
      (sexp)
      (list (typeof-sexp sexp)
            '()
            env
            subst))
    (op-exp
      (op rands)
      (let ((typeof-op (lambda (op-rand-types op-ret-type)
                         (list op-ret-type
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
               (typeof-op (v->lst rand-num 'int) 'int))
              ((memq op '(zero?))
               (typeof-op '(int) 'bool))
              ; list support; eopl 7.25
              ((eq? op 'list)
               (typeof-op (mapn (lambda (_) (typevar)) rand-num) 'list))
              ((eq? op 'car)
               (typeof-op '(list) (typevar)))
              ((eq? op 'cdr)
               (typeof-op '(list) 'list))
              ((eq? op 'cons)
               (typeof-op (list (typevar) 'list) 'list))
              (else
                (error 'typeof/subst "op:~s not supported" op)))))
    (lambda-exp
      (params body)
      (let* ((param-tvars (map (lambda (v) (typevar)) params))
             (lambda-tvar (typevar))
             (new-env (extend-envs params (newrefs param-tvars) env)))
        (match (typeof/subst body new-env subst)
          [(list body-type _ _ new-subst)
           (list lambda-tvar
                 '()
                 env
                 (unify new-subst
                        lambda-tvar
                        (proctype param-tvars
                           body-type)
                        exp))])))
    (if-exp
      (test then else)
      (match (typeof/subst test env subst)
        [(list test-type _ _ subst)
         (match (typeof/subst then env (unify subst test-type 'bool exp))
           [(list then-type _ _ subst)
            (match (typeof/subst else env subst)
              [(list else-type _ _ subst)
               (list then-type
                     '()
                     env
                     (unify subst then-type else-type exp))])])]))
    (letrec-exp
      (p-names procs body)
      (let* ((p-typevars (map (lambda (_) (typevar)) p-names))
             (new-env (extend-envs p-names (newrefs p-typevars) env)))
        (match (typeof-multi/subst procs new-env subst)
          [(list p-types subst)
           (match
             (typeof/subst body
                         new-env
                         (unify-multi subst p-typevars p-types exp)
                         )
             [(list body-type _ _ subst)
              (list body-type '() env subst)])])))
    (compound-exp
      (exps)
      (typeof-compound/subst exps env subst))
    (set-exp
      (var val)
      (match (typeof/subst val env subst)
        [(list val-type _ _ subst)
         (let* ((var-type (deref (apply-env env var)))
                (subst (unify subst var-type val-type exp)))
           (list 'void '() env subst))]))
    (define-exp
      (var val)
      (let* ((tvar (typevar))
             (new-env (extend-env var (newref tvar) env)))
        (match (typeof/subst val new-env subst)
          [(list val-type _ _ subst)
           (list 'void
                 (list var)
                 new-env
                 (unify subst tvar val-type exp))])))
    (module-exp
      (mname vars types bodies)
      (match (typeof-compound/subst bodies env subst)
        [(list _ body-vars new-env subst)
         (let ((mod-sig-type (modtype vars types))
               (mod-body-type (modtype body-vars (map (lambda (var)
                                   (deref (apply-env new-env var)))
                                 body-vars))))
           (let ((subst (unify subst mod-sig-type mod-body-type exp)))
             (let ((mod-type-var (typevar)))
               (list mod-type-var
                     (list mname)
                     (extend-env mname
                                 (newref mod-type-var)
                                 env)
                     (unify subst
                            mod-type-var
                            (modtype vars types)
                            exp)))))]))
    (import-exp
      (mname)
      (list 'void
            '()
            (import-mod mname env subst)
            subst))
    (call-exp
      (rator rands)
      (match (typeof/subst rator env subst)
        [(list rator-type _ _ subst)
         (let ((exp-tvar (typevar)))
           (list exp-tvar
                 '()
                 env
                 (match (typeof-multi/subst rands env subst)
                   [(list rand-types subst)
                    ;(printf "~a ~a\n" rator-type rand-types)
                    (unify subst rator-type (proctype rand-types exp-tvar) exp)])))]))
    ))

(define (import-mod mname env subst)
  (match (apply-subst-to-type subst (deref (apply-env env mname)))
    [`(mod type ,vars ,types)
      (extend-envs (map (lambda (var)
                          (sym-append mname ': var))
                        vars)
                   (newrefs types)
                   env)]))

(define (typeof exp)
  (initialize-store!)
  (match (typeof/subst (parse exp) (empty-env) '())
    [(list type _ _ subst)
     (apply-subst-to-type subst type)]))

(module+ test
  (require rackunit)
  (define (test-typeof exp)
    (type->str (typeof exp)))
  #|(test-typeof '(lambda (v) v))
  (test-typeof '(lambda (v)
             (zero? v)))|#

  ;(test-typeof '(lambda (f) (f 1)))
  ;(test-typeof '(define v 3))
  #|(test-typeof '(begin
                  (define v 3)
                  (lambda (v)
                    v)))|#
  #|(test-typeof '(module m1
                  (sig
                    (u int)
                    (f ((int) int)))
                  (body
                    (define u 3)
                    (define f (lambda (v) v)))))|#
  #|(test-typeof '(module m1
                  (sig
                    (u int))
                  (body
                    (define u 3)
                    (define v 5))))|#
  #|(test-typeof '(begin
                  (module m1
                    (sig
                      (u int)
                      (m2 (mod type
                            (v f)
                            (int ((int) int))
                            ))
                      (v int)
                      )
                    (body
                      (define u 3)
                      (module m2
                        (sig
                          (f ((int) int))
                          (v int))
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
                  ))|#
  )
