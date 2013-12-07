#lang eopl

(require "parser/parser-out.rkt")
(require "../base/utils.rkt")
(require "store.rkt")
(provide (all-defined-out))

#|(define-datatype
  expval expval?
  (numval
    (int number?))
  (boolval
    (bool boolean?))
  (strval
    (s string?))
  (procval
    (proc proc?))
  (listval
    (lst list?))
  (symval
    (sym symbol?))
  )

(define constval
  (lambda (v)
    (cond ((number? v) (numval v))
          ((string? v) (strval v))
          ((boolean? v) (boolval v))
          (else
            (eopl:error 'constval "~s is not a const value" v)))))

(define expval->normalval
  (lambda (val)
    (cases expval val
      (numval
        (num)
        num)
      (boolval
        (bool)
        bool)
      (procval
        (proc)
        proc)
      (listval
        (lst)
        (map expval->normalval lst))
      )))

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

(define expval->listval
  (lambda (val)
    (cases expval val
      (listval
        (lst)
        lst)
      (else
        (eopl:error 'expval->listval "~s is not a list" val)))))|#

(define-datatype
  proc proc?
  (closure
    (vars (list-of symbol?))
    (body tfexp?) 
    (env environment?)))

; environment

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (var (list-of symbol?))
    (ref (list-of reference?))
    (env environment?)))


(define apply-env
  (lambda (env search-var)
    ; (println "apply env:~s ~s" env search-var)
    (cases
      environment env
      (empty-env
        ()
        (eopl:error 'apply-env "var:~s not found" search-var))
      (extend-env
        (vars refs inherited-env)
        (let ((idx (index-of vars search-var)))
          (if (< idx 0)
            (apply-env inherited-env search-var)
            (list-ref refs idx))))
      )))
