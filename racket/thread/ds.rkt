#lang eopl
(require "grammar.rkt")
(require "store.rkt")
(require "../base/utils.rkt")
(require "thread.rkt")

(provide (all-defined-out))

(define-datatype
  expval expval?
  (numval
    (int integer?))
  (boolval
    (bool boolean?))
  (listval
    (list list?))
  (procval
    (proc proc?))
  (threadval
    (thread thread?)))

(define expval->normalval
  (lambda (val)
    (cases expval val
      (numval
        (num)
        num)
      (boolval
        (bool)
        bool)
      (listval
        (lst)
        (map expval->normalval lst))
      (procval
        (proc)
        proc)
      (threadval
        (thd)
        thd)
      )))

(define expval->listval
  (lambda (val)
    (cases expval val
      (listval
        (lst)
        lst)
      (else
        (eopl:error 'expval->listval "~s is not a list" val)))))

(define expval->numval
  (lambda (val)
    (cases expval val
      (numval
        (num)
        num)
      (else
        (eopl:error 'expval->numval "~s is not a num" val)))))

(define expval->boolval
  (lambda (val)
    (cases expval val
      (boolval
        (bool)
        bool)
      (else
        (eopl:error 'expval->boolval "~s is not a bool" val)))))
(define expval->procval
  (lambda (val)
    (cases expval val
      (procval
        (proc)
        proc)
      (else
        (eopl:error 'expval->procval "~s is not a proc" val)))))

(define-datatype
  proc proc?
  (closure
    (var symbol?)
    (body expression?) 
    (env environment?)))

; environment

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (ref reference?)
    (env environment?)))

(define extend-env-recursively
  (lambda (p-name b-var b-body env)
    (let* ((proc-ref (newref '()))
           (new-env (extend-env p-name
                                proc-ref
                                env)))
      (setref! proc-ref (procval (closure b-var
                                          b-body
                                          new-env)))
      new-env)))

(define apply-env
  (lambda (env search-var)
    (cases
      environment env
      (empty-env
        ()
        (eopl:error 'apply-env "var:~s not found" search-var))
      (extend-env
        (var val inherited-env)
        (if (eq? var search-var)
          val
          (apply-env inherited-env search-var)))
      )))


(define-datatype
  continuation continuation?
  (end-cont)
  (zero1-cont
    (cont continuation?))
  (let-exp-cont
    (var symbol?)
    (env environment?)
    (body expression?)
    (cont continuation?))
  (diff1-cont
    (subtractor-exp expression?)
    (env environment?)
    (cont continuation?))
  (diff2-cont
    ; todo (expval support)
    (minuend-val expval?)
    (env environment?)
    (cont continuation?))
  (if-test-cont
    (sbj-exp expression?)
    (else-exp expression?)
    (env environment?)
    (cont continuation?))
  (rator-cont
    (arg-exp expression?)
    (env environment?)
    (cont continuation?))
  (rand-cont
    (proc proc?)
    (env environment?)
    (cont continuation?))
  (cons1-cont
    (cdrv-exp expression?)
    (env environment?)
    (cont continuation?))
  (cons2-cont
    (carv expval?)
    (env environment?)
    (cont continuation?))
  (car-exp-cont
    (cont continuation?))
  (cdr-exp-cont
    (cont continuation?))
  (is-empty-exp-cont
    (cont continuation?))
  (multi-exp-cont
    (exps (list-of expression?))
    (accum-op procedure?)
    (accum expval?)
    (env environment?)
    (cont continuation?))
  (set-rhs-cont
    (ref reference?)
    (env environment?)
    (cont continuation?))
  (mult-cont1
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (mult-cont2
    (val expval?)
    (cont continuation?))
  (print-cont
    (cont continuation?))
  (spawn-cont
    (env environment?)
    (cont continuation?))
  )
