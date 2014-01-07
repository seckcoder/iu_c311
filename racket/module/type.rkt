#lang racket
; type check
(require eopl/datatype
         "parser.rkt"
         "../base/utils.rkt"
         "../types/type.rkt"
         "../types/store.rkt"
         "env.rkt")

(provide typeof)

(define (typeof-multi/subst exps env subst)
  (let loop ((rev-exp-types '())
             (subst subst)
             (exps exps))
    (if (null? exps)
      (list (reverse rev-exp-types) env subst)
      (match (typeof/subst (car exps) env subst)
        [(list exp-type _ subst)
         (loop (cons exp-type rev-exp-types) subst (cdr exps))]))))

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

; (type * type -> Equations)
(define match-type
  (lambda (type1 type2)
    (if (or (symbol? type1)
            (symbol? type2))
      (list (equa type1 type2))
      (match type1
        [(list (list var-types1 ...) ret-type1)
         (match type2
           [(list (list var-types2 ...) ret-type2)
            (foldl (lambda (subtype1 subtype2 acc)
                     (append acc
                             (match-type subtype1 subtype2)))
                   '()
                   (append var-types1 (list ret-type1))
                   (append var-types2 (list ret-type2)))])]))))

; unify ty1=ty2 to substitution
(define (unify subst ty1 ty2 exp)
  ; (print-subs subst)
  ; (printf "~a ~a\n" (prtype ty1) (prtype ty2))
  (let loop ((ty1 (apply-subst-to-type subst ty1))
             (ty2 (apply-subst-to-type subst ty2)))
    (cond ((equal? ty1 ty2)
           ; useless equation
           subst)
          ((and (simpletype? ty1)
                (simpletype? ty2))
           (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2))
          ((typevar? ty1)
           ; ty1 is typevar
           (if (occurs? ty1 ty2)
             (error 'unify "fail occurrence check for expression:~s" exp)
             (extend-subst subst ty1 ty2)))
          ((typevar? ty2)
           (loop ty2 ty1))
          ; both are not typevar and only one of them is simple type
          ((or (simpletype? ty1)
               (simpletype? ty2))
           (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2))
          ((and (proctype? ty1)
                (proctype? ty2))
            ; both are proc type
            (foldl (lambda (equation subst)
                     (match equation
                       [(list ty1 ty2)
                        (unify subst ty1 ty2 exp)]))
                   subst
                   (match-type ty1 ty2)))
          (
          )))

; substitution of a type for a type variable. type[sym = new-type]
(define replace
  (lambda (type sym new-type)
    ; replace every sym in type with new-type
    (match type
      [(? symbol? t)
       (if (eq? sym t)
         new-type
         t)]
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

(define (occurs? sym type)
  (match type
    [(? symbol? t) (eq? sym t)]
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

(define (typeof/subst exp env subst)
  (cases expression exp
    (const-exp
      (cst)
      (list (typeof-const cst)
            env
            subst))
    (var-exp
      (var)
      (list (deref (apply-env env var))
            env
            subst))
    (quote-exp
      (sexp)
      (list (typeof-sexp sexp)
            env
            subst))
    (op-exp
      (op rands)
      (let ((typeof-op (lambda (op-rand-types op-ret-type)
                         (list op-ret-type
                               env
                               (match (typeof-multi/subst rands env subst)
                                 [(list cur-rand-types _ subst)
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
          [(list body-type _ new-subst)
           (list lambda-tvar
                 env
                 (unify new-subst
                        lambda-tvar
                        (proctype param-tvars
                           body-type)
                        exp))])))
    (if-exp
      (test then else)
      (match (typeof/subst test env subst)
        [(list test-type _ subst)
         (match (typeof/subst then env (unify subst test-type 'bool exp))
           [(list then-type _ subst)
            (match (typeof/subst else env subst)
              [(list else-type _ subst)
               (list then-type
                     env
                     (unify subst then-type else-type exp))])])]))
    (letrec-exp
      (p-names procs body)
      (let* ((p-typevars (map (lambda (_) (typevar)) p-names))
             (new-env (extend-envs p-names (newrefs p-typevars) env)))
        (match (typeof-multi/subst procs new-env subst)
          [(list p-types _ subst)
           (match
             (typeof/subst body
                         new-env
                         (unify-multi subst p-typevars p-types exp)
                         )
             [(list body-type _ subst)
              (list body-type env subst)])])))
    (compound-exp
      (exps)
      (foldl (lambda (exp res)
               (match res
                 [(list _ env subst)
                  (typeof/subst exp env subst)]))
             (list 'void env subst)
             exps))
    (set-exp
      (var val)
      (match (typeof/subst val env subst)
        [(list val-type _ subst)
         (let* ((var-type (deref (apply-env env var)))
                (subst (unify subst var-type val-type exp)))
           (list 'void env subst))]))
    (define-exp
      (var val)
      (let* ((tvar (typevar))
             (new-env (extend-env var (newref tvar) env)))
        (match (typeof/subst val new-env subst)
          [(list val-type _ subst)
           (list 'void
                 new-env
                 (unify subst tvar val-type exp))])))
    (module-exp
      (mname vars1 types vars2 vals)
      ; get type of vals and unify them
      (match (typeof-multi/subst vals env subst)
        [(list val-types _ subst)
         (let ((type-pairs (map list vars2 vals val-types)))
           (let ((subst
                   (foldl
                     (lambda (var type subst)
                       (match (assq var type-pairs)
                         [#f
                          (error 'typeof "interface declaration ~s = ~s is not exist in module:~s"
                                 var type mname)]
                         [(list var2 val val-type)
                           (unify subst
                                  type
                                  val-type
                                  val)]))
                     subst
                     vars1
                     types)))
             (let ((mod-type-var (typevar)))
               (list mod-type-var
                     (extend-env mname
                                 (newref mod-type-var)
                                 env)
                     (unify subst
                            mod-type-var
                            (modtype vars1 types)
                            exp)))))]))
    (import-exp
      (mname)
      (list 'void
            (import-mod mname env)
            subst))
    (call-exp
      (rator rands)
      (match (typeof/subst rator env subst)
        [(list rator-type _ subst)
         (let ((exp-tvar (typevar)))
           (list exp-tvar
                 env
                 (match (typeof-multi/subst rands env subst)
                   [(list rand-types _ subst)
                    ;(printf "~a ~a\n" rator-type rand-types)
                    (unify subst rator-type (proctype rand-types exp-tvar) exp)])))]))
    ))

(define (import-mod mname env)
  (match (deref (apply-env env mname))
    [`(mod type ,vars ,types)
      (extend-envs (map (lambda (var)
                          (sym-append mname ': var))
                        vars)
                   (newrefs types)
                   env)]))

(define (typeof exp)
  (initialize-store!)
  (match (typeof/subst (parse exp) (empty-env) '())
    [(list type _ subst)
     (apply-subst-to-type subst type)]))
     #|(cond ((and (symbol? type)
                 (not (typevar? type)))
            type)
           ((and (not (null? subst)))
            (apply-subst-to-type subst type))
           (else
             (error "typeof failed")))]))|#

(module+ test
  (require rackunit)
  (define (test-typeof exp)
    (type->str (typeof exp)))
  #|(test-typeof '(lambda (v) v))
  (test-typeof '(lambda (v)
             (zero? v)))
  (test-typeof '(lambda (f)
             (f 1)))|#
  ;(test-typeof '(define v 3))
  #|(test-typeof '(begin
                  (define v 3)
                  (lambda (v)
                    v)))|#
  #|(test-typeof '(begin
                  (module m1
                    (sig
                      (u int)
                      )
                    (body
                      (u 3)))
                  (module m1
                    (sig
                      (u bool))
                    (body
                      (u #t)))
                  (import m1)
                  m1:u))|#
  (test-typeof '(begin
                  (module m1
                    (sig
                      (u int)
                      (m2 (mod type
                            (v f)
                            (int ((int) int))))
                      )
                    (body
                      (define u 3)
                      (module m2
                        (sig
                          (v int)
                          (f ((int) int)))
                        (body
                          (v 4)
                          (f (lambda (v)
                               (+ v 3)))))
                      ; no effect
                      (let ((f (lambda (v) v)))
                        (f u))
                      (import m2)
                      (define v m2:v)))
                  (import m1)
                  (import m1:m2)
                  (= m1:m2:v m1:v)))
  )
