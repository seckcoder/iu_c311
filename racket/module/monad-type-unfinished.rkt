#lang racket
; type check
(require eopl/datatype
         "../types/type.rkt"
         "../types/store.rkt"
         "env.rkt")

(provide type-check)

(define (typeof-multi/subst exps env subst)
  (let loop ((rev-exp-types '())
             (subst subst)
             (exps exps))
    (if (null? exps)
      (list (reverse rev-exp-types) subst )
      (match (typeof/subst (car exps) env subst)
        [(list exp-type subst)
         (loop (cons exp-type rev-exp-types) subst (cdr exps))]))))

(define (unify-multi subst types1 types2 exp)
  #|(println types1)(println types2)
  (println (length types1)) (println (length types2))|#
  (foldl (lambda (ty1 ty2 subst)
           (unify subst ty1 ty2 exp))
         subst
         types1
         types2))

(module Monad racket
  (provide unit
           bind
           combine
           pass)
  (define unit
    (lambda (t)
      ; monoid
      (lambda (env subst)
          `(,t ,env ,subst))))
  (define bind
    (lambda (ma f)
      ; monoid
      (lambda (env subst)
        (match (ma env subst)
          [(list a new-env new-subst)
           ((f a) new-env new-subst)]))))

  ; (ma * ma -> ma)
  (define combine
    (lambda (ma1 ma2)
      (lambda (env subst)
        (match (ma1 env subst)
          [(list a env subst)
           (ma2 env subst)]))))

  ; ((ma list) -> (a -> mb) -> mb)
  (define pass
    (lambda (mas g)
      (cond ((null? mas)
             (error 'pass "pass don't accept null list"))
            ((null? (cdr mas))
             (bind (car mas) g))
            (else
              (combine (bind (car mas) g)
                       (pass (cdr mas) g))))))
  )

(require (prefix-in Monad: 'Monad))

(define (typeof exp)
  (cases expression exp
    (const-exp
      (cst)
      (Monad:unit (typeof-const cst)))
    (var-exp
      (var)
      (lambda (env subst)
          (list (deref (apply-env env var))
                env
                subst))))
    (quote-exp
      (sexp)
      (Monad:unit (typeof-sexp sexp)))
    (op-exp
      (op rands)
      (cond ((memq op '(+ - * / =))
             (pass (map typeof rands)
                   (lambda (t)
                     (lambda (env subst)
                       (list t env (unify subst t 'int exp))))))
            (else
              (error 'typeof/subst "op:~s not supported" op))))
    (lambda-exp
      (params body)
      ;got a little confused here;
      ;TODO: read the essense of functional programming
      )
    (call-exp
      (rator rands)
      (match (typeof/subst rator env subst)
        [(list rator-type subst)
         (let ((exp-tvar (typevar)))
           (list exp-tvar
                 (match (typeof-multi/subst rands env subst)
                   [(list rand-types subst)
                    (unify subst rator-type (proctype rand-types exp-tvar) exp)])))]))
    (if-exp
      (test then else)
      (match (typeof/subst test env subst)
        [(list test-type subst)
         (match (typeof/subst then env (unify subst test-type 'bool exp))
           [(list then-type subst)
            (match (typeof/subst else env subst)
              [(list else-type subst)
               (list then-type
                     (unify subst then-type else-type exp))])])]))
    (letrec-exp
      (p-names procs body)
      (let* ((p-typevars (map (lambda (_) (typevar)) p-names))
             (new-env (extend-envs p-names (newrefs p-typevars) env)))
        (match (typeof-multi/subst procs new-env subst)
          [(list p-types subst)
           (typeof/subst body
                         new-env
                         (unify-multi subst p-typevars p-types exp)
                         )])))
    (compound-exp
      (exps)
      (foldl (lambda (exp res)
               (match res
                 [(list _ subst)
                  (typeof/subst exp env subst)]))
             (list 'void subst)
             exps))
    (else
      (error "not supported"))))

(define (type-check exp)
  (initialize-store!)
  (typeof exp (empty-env)))

(module+ test
  (require rackunit)

