#lang racket

(require eopl/datatype
         "parser.rkt"
         "../base/utils.rkt"
         "../types/type.rkt"
         "../types/store.rkt"
         "env.rkt")

(define-datatype
  proc proc?
  (closure
    (vars (list-of symbol?))
    (body expression?)
    (env environment?))
  )

(define apply-proc
  (lambda (rator rands)
    (cases proc rator
      (closure
        (vars body env)
        (let ((new-env (extend-envs vars
                                   (newrefs rands)
                                   env)))
          (>val (interp body new-env))))
      )))

(define extend-envs-recursively
  (lambda (p-names proc-exps env)
    (let* ((proc-refs (mapn (lambda (i)
                              (newref i))
                            (length proc-exps)))
           (new-env (extend-envs p-names
                                 proc-refs
                                 env)))
      (for-each (lambda (proc-ref proc)
                  (setref! proc-ref proc))
                proc-refs
                (map (lambda (proc)
                       (>val (interp proc env)))
                     proc-exps))
      new-env)))

(define >val car)
(define >env cadr)

(define (interp-multi-exps exps env)
  (foldl
    (lambda (exp acc)
      (interp exp (>env acc)))
    (list (void) env)
    exps))

(define (interp exp env)
  (cases expression exp
    (const-exp
      (cst) `(,cst ,env))
    (var-exp
      (var) `(,(deref (apply-env env var)) ,env))
    (quote-exp
      (sexp) `(,sexp ,env))
    (op-exp
      (op rand-exps)
      `(,(apply-base-ns op (map (lambda (exp)
                                  (>val (interp exp env)))
                                rand-exps))
        ,env))
    (call-exp
      (rator-exp rand-exps)
      (match (map (lambda (exp)
                    (>val (interp exp env)))
                  (cons rator-exp rand-exps))
           [(list rator rands ...)
            `(,(apply-proc rator rands) ,env)]))
    (if-exp
      (test then else)
      (if (interp test env)
        `(,(>val (interp then env)) env)
        `(,(>val (interp else env)) env)))
    (lambda-exp
      (vars body)
      `(,(closure vars body env) ,env))
    (compound-exp
      (exps)
      (interp-multi-exps exps env))
    (letrec-exp
      (p-names procs body)
      (let ((new-env (extend-envs-recursively p-names
                                              procs
                                              env)))
        `(,(>val (interp body new-env)) ,env)))
    (set-exp
      (var val)
      (list (setref! (apply-env env var)
                     (>val (interp val env)))
            env))
    (define-exp
      (var val)
      (list (void)
            (extend-env var (newref (>val (interp val env))) env)))
    (module-exp
      (mname vars types bodies)
      (match (interp-multi-exps bodies env)
        [(list _ new-env)
         (list (void)
               (extend-env mname
                           (newref (mod vars new-env))
                           env))]))
    (import-exp
      (mname)
      (list (void)
            (import-mod mname env)))
    ))

(define mod list)
(define (mod-bindings m)
  (match m
    [(list vars env)
     (list vars (map (lambda (var)
                       (apply-env env var))
                     vars))]))

; import mod mname and return a new env
(define (import-mod mname env)
  (let ((mod (deref (apply-env env mname))))
    (match (mod-bindings mod)
      [(list vars refs)
       (extend-envs (map (lambda (var)
                           (sym-append mname ': var))
                         vars)
                    refs
                    env)])))

(define (initial-env)
  (empty-env))

(define (test-prog exp)
  (initialize-store!)
  (>val (interp (parse exp) (initial-env))))

(module+ test
  (check =
    (test-prog  '(begin
                   (define a 3)
                   (module m1
                     (sig
                       (u int)
                       (f ((int) int)))
                     (body
                       ; module variable.
                       (define u a)
                       (set! u 4)
                       (define f (lambda (v)
                                   (+ u v)))
                       ; this expression has no effect on the whole module
                       (let ((v a))
                         (define f (lambda (v)
                                     (* u v)))
                         (f v))
                       (define g (lambda () u))
                       ))
                   (import m1)
                   (module m2
                     (sig
                       (v int)
                       (f ((int) int)))
                     (body
                       (define v a)
                       (define f (lambda (v)
                                   (* v v)))))
                   (set! m1:u 5)
                   (import m2)
                   (+ (m1:f m1:u)
                      (m2:f m2:v))
                ))
    19))
