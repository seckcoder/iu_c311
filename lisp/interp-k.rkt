; A tiny scheme interpreter in continuation passing style
#lang racket

(require eopl/datatype
         "parser.rkt"
         "../racket/base/utils.rkt"
         "store.rkt"
         "env.rkt")

(provide meval)

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
  (lambda (rator rands k dynamic-env)
    (cases proc rator
      (closure
        (vars body env)
        (let ((new-env (extend-envs vars
                                    (newrefs rands)
                                    ;env
                                    dynamic-env)))
          (interp/k body new-env k)))
      (cont
        (saved-k)
        (cond ((null? rands)
               (saved-k (void)))
              ((= (length rands) 1)
               (saved-k (car rands)))
              (else
                (error 'continuation "continuation receive zero or single argument"))))
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
                          (apply-proc rator rands k env)]))))
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
                       (k (tail lst-of-v)))))
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

  (test-prog '((lambda (a)
                 (((lambda (a)
                     (lambda (b)
                       (list a b))) 2) 3)
                 )
               1)
             '(2 3)
             "")
  )
