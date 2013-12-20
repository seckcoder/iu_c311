; A tiny scheme interpreter in continuation passing style
#lang racket

(require eopl/datatype
         "parser.rkt"
         "../racket/base/utils.rkt"
         "store.rkt"
         "env.rkt")

(provide meval)

(define extend-envs-recursively
  (lambda (p-names proc-exps env)
    (let* ((proc-refs (mapn (lambda (i)
                              (newref i))
                            (length proc-exps)))
           (new-env (extend-envs p-names
                                 proc-refs
                                 env)))
      (for-each (lambda (proc-ref proc-exp)
                  (setref! proc-ref (interp proc-exp new-env)))
                proc-refs proc-exps)
      new-env)))

(define-datatype
  proc proc?
  (closure
    (vars (list-of symbol?))
    (body expression?)
    (env environment?))
  )

(define apply-proc
  (lambda (rator rands dynamic-env)
    (cases proc rator
      (closure
        (vars body env)
        (let ((new-env (extend-envs vars
                                    (newrefs rands)
                                    env
                                    ;dynamic-env
                                    )))
          (interp body new-env)))
      )))

; InpExp * Env -> Res
(define (interp exp env)
  (cases expression exp
    (const-exp
      (cst) cst)
    (var-exp
      (var) (deref (apply-env env var)))
    (quote-exp
      (sexp) sexp)
    (op-exp
      (op rand-exps)
      (apply (eval op (make-base-namespace))
             (map (lambda (exp)
                    (interp exp env))
                  rand-exps)))
    (call-exp
      (rator-exp rand-exps)
      (cases expression rator-exp
        (var-exp
          (var) (deref (apply-env env var)))
        (lambda-exp
          (vars body)
          (interp body (extend-envs vars
                                    (
      (match (map (lambda (exp)
                    (interp exp env))
                  (cons rator-exp rand-exps))
        [(list rator rands ...)
         (apply-proc rator rands env)]))
    (if-exp
      (test then else)
      (let ((pred (interp test env)))
        (if pred
          (interp then env)
          (interp else env))))
    (lambda-exp
      (vars body)
      (closure vars body env))
    (compound-exp
      (exps)
      (foldl (lambda (exp acc)
               (interp exp env))
             (void)
             exps))
    (letrec-exp
      (p-names procs body)
      (extend-envs-recursively p-names
                               procs
                               env))
    (set-exp
      (var val-exp)
      (setref! (apply-env env var) (interp val-exp env))
      (void))
    (else
      (error 'interp "exp:~s cannot be evaluated" exp))
    ))

(define (interp-sexp sexp env)
  (interp (parse sexp) env))


(define (meval sexp)
  (initialize-store!)
  (interp-sexp sexp (empty-env))
  )

(module+ test
  (require rackunit)
  (define test-prog
    (lambda args
      (match args
        [(list) (void)]
        [(list prog expected desc rest ...)
         (check-equal? (meval prog) expected desc)
         (apply test-prog rest)])))

  (test-prog '((lambda (a)
                 (((lambda (a)
                     (lambda (b)
                       (list a b))) 2) 3)
                 )
               1)
             '(2 3)
             "")
  )
