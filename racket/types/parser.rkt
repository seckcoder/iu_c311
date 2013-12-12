#lang racket

(require eopl/datatype
         "../base/utils.rkt"
         "../cps/builtin.rkt")

(provide (all-defined-out))

(define-datatype
  expression expression?
  (const-exp
    (cst const?))
  (var-exp
    (var symbol?))
  (quote-exp
    (sexp sexp?))
  (op-exp
    (op op?)
    (params (list-of expression?)))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  (if-exp
    (test expression?)
    (then expression?)
    (else expression?))
  (lambda-exp
    (vars (list-of symbol?))
    (types (list-of symbol?))
    (body expression?))
  (compound-exp
    (exps (list-of expression?)))
  (letrec-exp
    (p-names (list-of symbol?))
    (ret-types (list-of symbol?))
    (procs (list-of expression?))
    (body expression?))
  )

(define (parse-multi exps)
  (map (lambda (exp)
         (parse exp))
       exps))

(define (single-or-compound exps)
  (if (null? (cdr exps))
    (car exps)
    `(begin
       ,@exps)))

(define (parse sexp)
  (match sexp
    [(? const? x) (const-exp x)]
    [(? symbol? x) (var-exp x)]
    ; symbol
    [`(quote ,x) (quote-exp x)]
    ; builtin ops
    [(list (? op? op) params* ...)
     (op-exp op (parse-multi params*))]
    ; if 
    [`(if ,test ,then)
      (parse `(if ,test
                ,then
                (void)))]
    [`(if ,test ,then ,else)
      (if-exp (parse test)
              (parse then)
              (parse else))]
    ; lambda
    [`(lambda ((,rands ,types)...) ,body ,bodies* ...)
      (lambda-exp rands
                  types
                  (parse (single-or-compound (cons body bodies*))))]
    [`(begin ,body ,bodies* ...)
      (compound-exp (parse-multi (cons body bodies*)))]
    #|[`(let ((,var ,val) ...) ,body ,bodies* ...)
      (parse `((lambda (,@var)
                 ,@(cons body bodies*))
               ,@val))]|#
    [`(letrec (((,name* ,ret-type*) ,proc*) ...) ,body ,bodies* ...)
      (letrec-exp name*
                  ret-type*
                  (parse-multi proc*)
                  (parse (single-or-compound (cons body bodies*))))]
    [`(cond (,pred* ,body*) ...)
      (if (not (null? pred*))
        (let ((pred (car pred*))
              (body (car body*)))
          (cond ((and (eq? pred 'else)
                      (null? (cdr pred*)))
                 (parse body))
                ((eq? pred 'else)
                 (error 'parse "cond else should be the last expression"))
                (else
                  (parse `(if ,pred
                            ,body
                            (cond ,@(map (lambda (pred body)
                                           (list pred body))
                                         (cdr pred*)
                                         (cdr body*))))))))
        '(void))
      ]
    ; procedure call
    [(list rand rators ...)
     (call-exp (parse rand)
               (map (lambda (rator)
                      (parse rator))
                    rators))]
    ))
