#lang racket

(require eopl/datatype
         "../../base/utils.rkt"
         "../store.rkt")

(provide (all-defined-out))

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (vars (list-of symbol?))
    (refs (list-of reference?))
    (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases
      environment env
      (empty-env
        ()
        (error 'apply-env "var:~s not found" search-var))
      (extend-env
        (vars refs inherited-env)
        (match (find (lambda (var)
                       (eq? var search-var))
                     vars)
          [(list finded? var pos rest ...) 
           (if finded?
             (list-ref refs pos)
             (apply-env inherited-env search-var))]))
      )))
