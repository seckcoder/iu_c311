; A tiny scheme interpreter in continuation passing style
#lang racket

(require eopl/datatype
         "../../parser.rkt"
         "../../base/utils.rkt"
         "../store.rkt"
         "env.rkt")

(define extend-envs-recursively
  (lambda (p-names proc-exps env k)
    (let* ((proc-refs (mapn (lambda (i)
                              (newref i))
                            (length proc-exps)))
           (new-env (extend-envs p-names
                                 proc-refs
                                 env)))
      (interp-exps/k proc-exps
                     new-env
                     (lambda (lst-of-proc)
                       (for-each (lambda (proc-ref proc)
                                   (setref! proc-ref proc))
                                 proc-refs lst-of-proc)
                       (k new-env))))))

(define-datatype
  proc proc?
  (closure
    (vars (list-of symbol?))
    (body expression?)
    (env environment?))
  (cont
    (k procedure?))
  )
(define apply-proc
  (lambda (rator rands k)
    (cases proc rator
      (closure
        (vars body env)
        (let ((new-env (extend-envs vars
                                   (newrefs rands)
                                   env)))
          (interp/k body new-env k)))
      (cont
        (saved-k)
        (saved-k (car rands)))
      )))

; InpExps * Env * ((Res)->Res) -> (list-of Res)
(define (interp-exps/k exps env k)
  (if (null? exps)
    (k '())
    (interp/k (car exps)
            env
            (lambda (v)
              (interp-exps/k (cdr exps)
                             env
                             (lambda (lst-of-v)
                               (k (cons v lst-of-v))))))))

; InpExp * Env * ((Res)->Res) -> Res
(define (interp/k exp env k)
  (cases expression exp
    (const-exp
      (cst) (k cst))
    (var-exp
      (var) (k (deref (apply-env env var))))
    (quote-exp
      (sexp) (k sexp))
    (op-exp
      (op rand-exps)
      (interp-exps/k rand-exps
                     env
                     (lambda (rands)
                       (k (apply (eval op (make-base-namespace))
                                 rands)))))
    (call-exp
      (rator-exp rand-exps)
      (interp-exps/k (cons rator-exp rand-exps)
                     env
                     (lambda (lst-of-v)
                       (match lst-of-v
                         [(list rator rands ...)
                          (apply-proc rator rands k)]))))
    (if-exp
      (test then else)
      (interp/k test env (lambda (test-v)
                           (if test-v
                             (interp/k then env k)
                             (interp/k else env k)))))
    (lambda-exp
      (vars body)
      (k (closure vars body env)))
    (compound-exp
      (exps)
      (interp-exps/k exps
                     env
                     (lambda (lst-of-v)
                       (k (car (list-tail lst-of-v 0))))))
    (letrec-exp
      (p-names procs body)
      (extend-envs-recursively p-names
                               procs
                               env
                               (lambda (new-env)
                                 (interp/k body new-env k))))
    (letcc-exp
      (var body)
      (let ((new-env (extend-env var
                                 (newref (cont k))
                                 env)))
        (interp/k body new-env k)))
    (set-exp
      (var val-exp)
      (interp/k val-exp env (lambda (val)
                              (setref! (apply-env env var)
                                       val)
                              (k (void)))))
    (define-exp
      (var val-exp)
      ; current implementation of define is apparently not right.
      ; looking forward to better method
      (interp/k val-exp env (lambda (val)
                              (k (extend-env var (newref val) env)))))
    ))

(define (interp exp env)
  (interp/k exp env (lambda (v) v)))

(define (interp-sexp sexp env)
  (interp (parse sexp) env))

(define initial-env
  (lambda defines
    (let loop ((defines defines)
               (env (empty-env)))
      (if (null? defines)
        env
        (loop (cdr defines)
              (interp-sexp (car defines) env))))))

(define (meval sexp)
  (initialize-store!)
  (interp-sexp sexp (initial-env '(define call/cc (lambda (p)
                                                    (let/cc k
                                                      (p k))))))
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
  (test-prog '3 3 "number")
  (test-prog '(+ 1 2) 3 "op")
  (test-prog '((lambda (v)
                 v)
               3) 3 "lambda")
  (test-prog '(let ((v 3))
                v) 3 "let")
  (test-prog '(let ((foo (lambda (v)
                           v)))
                (if (= (foo 3) 3)
                  (foo 4)
                  (foo 5))) 4 "if")
  (test-prog '(letrec ((fact (lambda (n)
                               (if (= n 0)
                                 1
                                 (* n (fact (- n 1)))))))
                (fact 4))
             24 "letrec")
  (test-prog '(- 3 (let/cc k
                     (k 2))) 1 "letcc")
  (test-prog '(- 3 (call/cc (lambda (k) (k 2))))
             1 "call/cc")
  )
