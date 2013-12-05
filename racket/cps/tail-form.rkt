#lang eopl

(require "../base/utils.rkt")
(require "ds.rkt")
(require "store.rkt")
(require "grammar.rkt")

(define extend-env-recursively
  (lambda (p-names proc-exps inherited-env)
    (let* ((refs (newrefs (mapn (lambda (_) '()) (length p-names))))
           (new-env (extend-env p-names refs inherited-env)))
      (for-each (lambda (ref pname proc-exp)
                  (setref! ref (value-of-simple-exp proc-exp new-env)))
                refs
                p-names
                proc-exps)
      new-env)))

; 6.11
(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-exp exp
      (const-exp
        (val)
        val)
      (var-exp
        (var)
        (deref (apply-env env var)))
      (atom-exp
        (at)
        at)
      (list-exp
        (exps)
        (map (lambda (exp)
               (value-of-simple-exp exp env))
             exps))
      (op-exp
        (op exps)
        (apply (eval op) (map (lambda (exp)
                                (value-of-simple-exp exp env))
                              exps)))
      (lambda-exp
        (vars body)
        (closure vars body env))
      )))

(define interp-exp/k
  (lambda (exp env)
    (cases tfexp exp
      (simple-exp->exp
        (simple)
        (value-of-simple-exp simple env))
      (cps-letrec-exp
        (p-names proc-exps letrec-body)
        (let ((new-env (extend-env-recursively p-names
                                               proc-exps
                                               env)))
          (interp-exp/k letrec-body new-env)))
      (cps-if-exp
        (pred body-yes body-no)
        (if (value-of-simple-exp pred env)
          (interp-exp/k body-yes env)
          (interp-exp/k body-no env)))
      (cps-compound-exp
        (simple-exps a-tfexp)
        ; If a language doesn't allow side-effect,
        ; then what's the usage of compound expression?
        ; Hmm, interesting...
        (map (lambda (exp)
               (value-of-simple-exp exp env))
             simple-exps)
        (interp-exp/k a-tfexp env))
      (cps-call-exp
        (rator rands)
        (let ((rator-proc (value-of-simple-exp rator env))
              (rand-vals
                (map (lambda (simple)
                       (value-of-simple-exp simple env))
                     rands)))
        (apply-proc rator-proc rand-vals)))
      )))

(define apply-proc
  (lambda (rator rands)
    (cases proc rator
      (closure
        (vars body env)
        (let ((new-env (extend-env vars
                                   (newrefs rands)
                                   env)))
          (interp-exp/k body new-env))))))

(define interp
  (lambda (exp)
    (interp-exp/k exp
                  (empty-env))))

(provide (all-defined-out))
