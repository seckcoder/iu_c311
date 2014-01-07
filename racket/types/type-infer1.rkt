#lang racket

(require eopl/datatype
         "store.rkt"
         "env.rkt"
         "infer-parser1.rkt"
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

; 'int 'bool ...
(define (simpletype? t)
  (and (symbol? t)
       (not (typevar? t))))

(define (typeof exp)
  (initialize-store!)
  (match (typeof/subst exp (empty-env) '())
    [(list type subst)
     ;(print-subs subst)
     (cond ((and (symbol? type)
                 (not (typevar? type)))
            type)
           ((and (not (null? subst)))
            (apply-subst-to-type subst type))
           (else
             (error "typeof failed")))]))

(define (typeof-multi/subst exps env subst)
  (let loop ((rev-exp-types '())
             (subst subst)
             (exps exps))
    (if (null? exps)
      (list (reverse rev-exp-types) subst )
      (match (typeof/subst (car exps) env subst)
        [(list exp-type subst)
         (loop (cons exp-type rev-exp-types) subst (cdr exps))]))))

(define (unify-multi subst types1 types2 exp)
  #|(println types1)(println types2)
  (println (length types1)) (println (length types2))|#
  (foldl (lambda (ty1 ty2 subst)
           (unify subst ty1 ty2 exp))
         subst
         types1
         types2))

(define (typeof/subst exp env subst)
  (cases expression exp
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
      (let ((typeof-op (lambda (op-rand-types op-ret-type)
                         (list op-ret-type
                               (match (typeof-multi/subst rands env subst)
                                 [(list cur-rand-types subst)
                                  (unify-multi subst
                                               cur-rand-types
                                               op-rand-types
                                               exp)]))))
            (rand-num (length rands)))
        (cond ((memq op '(+ - * / =))
               (typeof-op (v->lst rand-num 'int) 'int))
              ((memq op '(zero?))
               (typeof-op '(int) 'bool))
              ; list support; eopl 7.25
              ((eq? op 'list)
               (typeof-op (mapn (lambda (_) (typevar)) rand-num) 'list))
              ((eq? op 'car)
               (typeof-op '(list) (typevar)))
              ((eq? op 'cdr)
               (typeof-op '(list) 'list))
              ((eq? op 'cons)
               (typeof-op (list (typevar) 'list) 'list))
              (else
                (error 'typeof/subst "op:~s not supported" op)))))
    (lambda-exp
      (params body)
      (let* ((param-tvars (map (lambda (v) (typevar)) params))
             (new-env (extend-envs params (newrefs param-tvars) env)))
        (match (typeof/subst body new-env subst)
          [(list body-type new-subst)
           (list (proctype param-tvars
                           body-type)
                 new-subst)])))
    (call-exp
      (rator rands)
      (match (typeof/subst rator env subst)
        [(list rator-type subst)
         (let ((exp-tvar (typevar)))
           (list exp-tvar
                 (match (typeof-multi/subst rands env subst)
                   [(list rand-types subst)
                    (unify subst rator-type (proctype rand-types exp-tvar) exp)])))]))
    (if-exp
      (test then else)
      (match (typeof/subst test env subst)
        [(list test-type subst)
         (match (typeof/subst then env (unify subst test-type 'bool exp))
           [(list then-type subst)
            (match (typeof/subst else env subst)
              [(list else-type subst)
               (list then-type
                     (unify subst then-type else-type exp))])])]))
    (let-exp
      (var val-exp body)
      ;TODO: implement let polymorphism
      (match (typeof/subst val-exp env subst)
        [(list val-type subst)
         (let ((new-env (extend-env var (newref val-type) env)))
           (typeof/subst body new-env subst))]))
    (letrec-exp
      (p-names procs body)
      (let* ((p-typevars (map (lambda (_) (typevar)) p-names))
             (new-env (extend-envs p-names (newrefs p-typevars) env)))
        (match (typeof-multi/subst procs new-env subst)
          [(list p-types subst)
           (typeof/subst body
                         new-env
                         (unify-multi subst p-typevars p-types exp)
                         )])))
    ))

; apply subst to type(replace type with bindings)
; or lookup type in subst
(define (apply-subst-to-type subst type)
  (if (simpletype? type)
    type  ; a tiny optimization. only apply for non simpletype(eopl 7.20)
    (foldl (lambda (subst-equa type)
             (match subst-equa
               [(list tyvar tybind)
                (replace type tyvar tybind)]))
           type
           subst)))

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
  ; (print-subs subst)
  ; (printf "~a ~a\n" (prtype ty1) (prtype ty2))
  (let loop ((ty1 (apply-subst-to-type subst ty1))
             (ty2 (apply-subst-to-type subst ty2)))
    (cond ((equal? ty1 ty2)
           ; useless equation
           subst)
          ((and (simpletype? ty1)
                (simpletype? ty2)
                (not (eq? ty1 ty2)))
           (error 'unify "type error for expression:~s" exp))
          ((typevar? ty1)
           ; ty1 is typevar
           (if (occurs? ty1 ty2)
             (error 'unify "fail occurrence check for expression:~s" exp)
             (extend-subst subst ty1 ty2)))
          ((typevar? ty2)
           (loop ty2 ty1))
          (else
            ; both are proc type
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


; test code ========================

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
  #|(test-typeof '(lambda (f)
                  (lambda (x)
                    (- (f 3) (f x)))))
  (test-typeof '(lambda (x)
                  (lambda (y)
                    (x y))))
  (test-typeof '(lambda (v)
                  (if (zero? v)
                    (+ v 2)
                    v)))
  (test-typeof '(letrec ((double (lambda (v)
                                   (if (zero? v)
                                     0
                                     (* (double (- v 1)) 2)))))
                  (double 3)))
  (test-typeof '(letrec ((even (lambda (n)
                                 (if (zero? n)
                                   #t
                                   (odd (- n 1)))))
                         (odd (lambda (n)
                                (if (zero? n)
                                  #f
                                  (even (- n 1))))))
                  (odd 3)))

  (test-typeof '(lambda (lst)
                  (+ (car lst) 2)))
  (test-typeof '(lambda (lst)
                  (cdr lst)))
  (test-typeof '(lambda (lst v)
                  (cons (+ v 2) '())))
  (test-typeof '(lambda (v)
                  (list v)))|#
  ; fail occurrence check
  #|(test-typeof '(lambda (f)
                    (f f)))|#
  #|(test-typeof '(lambda (f)
                  (+ (f #t) (f 1))))|#
  ; let polymorphism
  (test-typeof 
    '(let ((f (lambda (v) v)))
       (list (f 1) (f #f))))

  ; another test case for let polymorphism
  (lambda (g)
    (let ((f g))
      (list (f 1) (f #f))))
  )
