#lang racket

(require eopl/datatype
         "store.rkt"
         "env.rkt"
         "infer-parser.rkt"
         "../base/utils.rkt")

(define (typeof-const v)
  (cond ((number? v) 'int)
        ((string? v) 'str)
        ((boolean? v) 'bool)))

(define (typeof-sexp v)
  (cond ((atom? v) 'atom)
        ((list? v) 'list)))

; get printable version of type names
(define (pr t)
  (match t
    [(? symbol? t) t]
    [(list (list var-types ...) ret-type)
     (format "~s -> ~s" (map pr var-types) (pr ret-type))]
    [_ (error 'pr "pr match error:~s" t)]))

(define (check-equal-type! t1 t2 exp)
  (if (not (equal? t1 t2))
    (error 'Type-Match "~s not match ~s in ~a" (pr t1) (pr t2) exp)
    (void)))


(define typevar
  (let ((n -1))
    (lambda ()
      (set! n (+ n 1))
      (string->symbol (string-append "t" (number->string n)))
      )))

(define (equa left right)
  (list left right))

(define equa->left car)
(define equa->right cadr)

(define (proctype var-type ret-type)
  `((,var-type) ,ret-type))

(define unknown?
  (lambda (type)
    (and (symbol? type)
         (char=? (string-ref (symbol->string type) 0)
                 #\t))))

(define (analyze sexp env)
  (cases expression sexp
    (const-exp
      (cst)
      (list (typeof-const cst)
            '()))
    (var-exp
      (var)
      (list (deref (apply-env env var)) '()))
    (quote-exp
      (sexp)
      (list (typeof-sexp sexp)
            '()))
    (op-exp
      (op rands)
      (let ((analyze-op (lambda (rand-type ret-type)
                          (list ret-type
                                (foldl (lambda (rand acc)
                                         (match (analyze rand env)
                                           [(list rand-type equations)
                                            `(,@acc
                                              ,@equations
                                              ,(equa rand-type rand-type))]))
                                       '()
                                       rands)))))
        (cond ((memq op '(+ - * / =))
               (analyze-op 'int 'int))
              ((memq op '(zero? number?))
               (analyze-op 'int 'bool))
              (else
                (error 'analyze "op:~s not supported" op)))))
    (lambda-exp
      (param body)
      (let* ((param-tvar (typevar))
             (new-env (extend-env param (newref param-tvar) env)))
        (match (analyze body new-env)
          [(list ret-type equations)
           (let ((lambda-tvar (typevar)))
             (list lambda-tvar
                   (cons (equa lambda-tvar (proctype param-tvar ret-type))
                         equations)))])))
    (call-exp
      (rator rand)
      (match (analyze rator env)
        [(list rator-type equations1)
         (match (analyze rand env)
           [(list rand-type equations2)
            (let ((call-tvar (typevar)))
              (list call-tvar
                    `(,@equations1
                      ,@equations2
                      ,(equa rator-type (proctype rand-type call-tvar)))))]
           )]))
    (else
      (error "not supported"))
    ))

(define (real-typeof sexp)
  (initialize-store!)
  (match (analyze sexp (empty-env))
    [(list type equations)
     (solve equations)]))

(define (pr-equa equation)
  (format "~a = ~a"
          (pr (equa->left equation))
          (pr (equa->right equation))))

(define (solve equas)
  (for-each (lambda (equation)
              (println (pr-equa equation)))
            equas))


(module+ test
  (real-typeof (parse '(lambda (v)
                         (zero? v))))
  (real-typeof (parse '(lambda (f)
                         (f 1))))
  )
