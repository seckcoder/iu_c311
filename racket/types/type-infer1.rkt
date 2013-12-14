#lang racket

(require eopl/datatype
         "store.rkt"
         "env.rkt"
         "infer-parser.rkt"
         "../base/utils.rkt")

(define (proctype var-type ret-type)
  `(,var-type ,ret-type))

(define (typeof-const v)
  (cond ((number? v) 'int)
        ((string? v) 'str)
        ((boolean? v) 'bool)
        (else #f)))

(define (typeof-sexp v)
  (cond ((atom? v) 'atom)
        ((list? v) 'list)
        (else #f)))

(define typevar
  (let ((n -1))
    (lambda ()
      (set! n (+ n 1))
      (string->symbol (string-append "t" (number->string n)))
      )))

(define (equa left right)
  (list left right))

(define typevar?
  (lambda (type)
    (and (symbol? type)
         (char=? (string-ref (symbol->string type) 0)
                 #\t))))

(define (typeof exp)
  (initialize-store!)
  (match (typeof/subst exp (empty-env) '())
    [(list type subst)
     (cond ((and (symbol? type)
                 (not (typevar? type)))
            type)
           ((and (not (null? subst)))
            (apply-subst-to-type subst type))
           (else
             (error "typeof failed")))]))

(define (typeof/subst sexp env subst)
  (cases expression sexp
    (const-exp
      (cst)
      (list (typeof-const cst) subst))
    (var-exp
      (var)
      (list (deref (apply-env env var)) subst))
    (quote-exp
      (sexp)
      (list (typeof-sexp sexp) subst))
    (op-exp
      (op rands)
      (let ((typeof-op (lambda (op-rand-type op-ret-type)
                         (list op-ret-type
                               (foldl (lambda (rand subst)
                                        (match (typeof/subst rand env subst)
                                          [(list cur-rand-type new-subst)
                                           (unify new-subst cur-rand-type op-rand-type exp)]))
                                      subst
                                      rands)))))
        (cond ((memq op '(+ - * / =))
               (typeof-op 'int 'int))
              ((memq op '(zero? number?))
               (typeof-op 'int 'bool))
              (else
                (error 'typeof/subst "op:~s not supported" op)))))
    (lambda-exp
      (param body)
      (let* ((param-tvar (typevar))
             (new-env (extend-env param (newref param-tvar) env)))
        (match (typeof/subst body new-env subst)
          [(list body-type new-subst)
           (list (proctype (list param-tvar)
                           body-type)
                 new-subst)])))
    (call-exp
      (rator rand)
      (match (typeof/subst rator env subst)
        [(list rator-type subst)
         (match (typeof/subst rand env subst)
           [(list rand-type subst)
            (let ((exp-tvar (typevar)))
              (list exp-tvar
                    (unify subst rator-type (proctype (list rand-type) exp-tvar) exp)))])]))
    (else
      (error "..."))
    ))

; apply subst to type(replace type with bindings)
(define (apply-subst-to-type subst type)
  (foldl (lambda (subst-equa type)
           (match subst-equa
             [(list tyvar tybind)
              (replace type tyvar tybind)]))
         type
         subst))

; (type * type -> Equations)
(define match-type
  (lambda (type1 type2)
    (if (or (symbol? type1)
            (symbol? type2))
      (list (equa type1 type2))
      (match type1
        [(list (list var-types1 ...) ret-type1)
         (match type2
           [(list (list var-types2 ...) ret-type2)
            (foldl (lambda (subtype1 subtype2 acc)
                     (append acc
                             (match-type subtype1 subtype2)))
                   '()
                   (append var-types1 (list ret-type1))
                   (append var-types2 (list ret-type2)))])]))))

; unify ty1=ty2 to substitution
(define (unify subst ty1 ty2 exp)
  ; (printf "~a ~a\n" (prtype ty1) (prtype ty2))
  (let loop ((ty1 (apply-subst-to-type subst ty1))
             (ty2 (apply-subst-to-type subst ty2)))
    (cond ((equal? ty1 ty2)
           ; useless equation
           subst)
          ((typevar? ty1)
           ; ty1 is typevar
           (if (occurs? ty1 ty2)
             (error 'unify "fail occurrence check for expression:~s" exp)
             (extend-subst subst ty1 ty2)))
          ((typevar? ty2)
           (loop ty2 ty1))
          (else
            (foldl (lambda (equation subst)
                     (match equation
                       [(list ty1 ty2)
                        (unify subst ty1 ty2 exp)]))
                   subst
                   (match-type ty1 ty2))))))

; substitution of a type for a type variable. type[sym = new-type]
(define replace
  (lambda (type sym new-type)
    ; replace every sym in type with new-type
    (match type
      [(? symbol? t)
       (if (eq? sym t)
         new-type
         t)]
      [(list (list var-types ...) ret-type)
       (proctype (map (lambda (sub-type)
                        (replace sub-type sym new-type))
                      var-types)
                 (replace ret-type sym new-type))])))

; subst[ty1=ty2] replace subst's bindings with ty2
(define (extend-subst subst ty1 ty2)
  (cons (equa ty1 ty2)
        (map (lambda (subst-equa)
               (match subst-equa
                 [(list tyvar tybind)
                  (equa tyvar (replace tybind ty1 ty2))]))
             subst)))

(define (occurs? sym type)
  (match type
    [(? symbol? t) (eq? sym t)]
    [(list (list var-types ...) ret-type)
     (ormap (lambda (t)
              (occurs? sym t))
            (cons ret-type var-types))]))

; get printable version of type names
(define (prtype t)
  (match t
    [(? symbol? t) t]
    [(list (list var-types ...) ret-type)
     (format "~a -> ~a" (map prtype var-types) (prtype ret-type))]
    [_ (error 'prtype "match error:~s" t)]))
(define (pr-equa equation)
  (match equation
    [(list left right)
     (format "~a = ~a"
             (prtype left)
             (prtype right))]))
(define (print-equations equas)
  (for-each (lambda (equation)
              (println (pr-equa equation)))
            equas)
  (newline)
  )

(define (print-subs subs)
  (println "subs:")
  (print-equations subs))

(define (print-type type)
  (println (prtype type)))



(module+ test
  (define (test-typeof sexp)
    (prtype (typeof (parse sexp))))
  #|(test-typeof '(lambda (v)
                    (zero? v)))|#
  #|(test-typeof '(lambda (f)
                    (f 1)))|#
  (test-typeof '(lambda (f)
                  (lambda (x)
                    (- (f 3) (f x)))))
  (test-typeof '(lambda (x)
                  (lambda (y)
                    (x y))))
  )
