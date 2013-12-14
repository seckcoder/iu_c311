#lang racket

(require eopl/datatype
         "store.rkt"
         "env.rkt"
         "infer-parser.rkt"
         "../base/utils.rkt")

(define (typeof-const v)
  (cond ((number? v) 'int)
        ((string? v) 'str)
        ((boolean? v) 'bool)
        (else #f)))

(define (typeof-sexp v)
  (cond ((atom? v) 'atom)
        ((list? v) 'list)
        (else #f)))

(define (proc? t)
  (match t
    [(list (list var-types ...) ret-type) #t]
    [_ #f]))

(define (simple-type? t)
  (symbol? t))

; get printable version of type names
(define (pr t)
  (match t
    [(? symbol? t) t]
    [(list (list var-types ...) ret-type)
     (format "~a -> ~a" (map pr var-types) (pr ret-type))]
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
  `(,var-type ,ret-type))

(define type-var?
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
                                           [(list rand-var-type equations)
                                            `(,@acc
                                               ,@equations
                                               ,(equa rand-var-type rand-type))]))
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
                   `(,@equations
                      ,(equa lambda-tvar (proctype (list param-tvar) ret-type)))))])))
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
                       ,(equa rator-type (proctype (list rand-type) call-tvar)))))]
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

(define (print-equations equas)
  (for-each (lambda (equation)
              (println (pr-equa equation)))
            equas)
  (newline)
  )

(define (print-subs subs)
  (println "subs:")
  (print-equations subs))

(define (occurs? sym type)
  (match type
    [(? symbol? t) (eq? sym t)]
    [(list (list var-types ...) ret-type)
     (ormap (lambda (t)
              (occurs? sym t))
            (cons ret-type var-types))]))

; (Equation -> Equations)
; Rule: int = ta -> ta = int
;       ta = ta -> dismiss
;       int = int -> dismiss
;       ((int) -> int) = ((a) -> int) -> a = int;
(define (normalize-equa equation)
  (match equation
    [(list left right)
     (cond ((and (symbol? left)
                 (symbol? right)
                 (eq? left right))
            '())
           ((type-var? left)
            (if (occurs? left right)
              (error 'verify-equa "fail occurence check")
              (list equation)))
           ((type-var? right)
            (normalize-equa (equa right left)))
           (else
             (let ((equas (match-type left right)))
               (flatmap normalize-equa equas))))]))

(define equa->sub
  (lambda (equation subs)
    ; (Equation * Substitions) -> (Equations Substitions)
    ; Send an equation to substitions. It's possible that
    ; the equation is turned to substition. It's also possible
    ; that the equation is degraded into multi equations.

    ; use for loops here
    (let loop ((equation equation)
               (new-subs '())
               (old-subs subs))
      #|(println equation)
      (print-subs new-subs)
      (print-subs old-subs)|#
      (if (null? old-subs)
        ; move the equation to new-subs
        (list '() (append new-subs (list equation)))
        (let ((sub-equa (car old-subs)))
          (match equation
            [(list left1 right1)
             (match sub-equa
               [(list left2 right2)
                (cond ((eq? left1 left2)
                       #|(printf "Degrade: \n ~a \n ~a \n"
                               (pr-equa equation)
                               (pr-equa sub-equa))
                       (println (degrade equation sub-equa))|#
                       ; degrade the equation
                       (list (degrade equation sub-equa)
                             subs))
                      ((occurs? left2 right1)
                       ; replace equation's right with substitution's right
                       (loop (equa left1 (replace1 right1 left2 right2))
                             (append new-subs (list sub-equa))
                             (cdr old-subs)))
                      ((occurs? left1 right2)
                       ; replace subsititution's right with equation's right
                       (loop equation
                             (append new-subs (list (equa left2
                                                          (replace1 right2 left1 right1))))
                             (cdr old-subs)))
                      (else
                        ; just next
                        (loop equation
                              (append new-subs (list sub-equa))
                              (cdr old-subs))))])]))))))

#|(define replace
  (lambda (equation subs)
    ; (Equation Substitions) -> Substitions
    ; replace every substition and move equation to subs
    (match equation
      [(list left1 right1)
       (append (map (lambda (sub)
                      (match sub
                        [(list left2 right2)
                         (equa left2 (replace1 right2 left1 right1))]))
                    subs)
               (list equation))])))|#
(define replace1
  (lambda (type sym new-type)
    ; replace every sym in type with new-type
    (match type
      [(? symbol? t)
       (if (eq? sym t)
         new-type
         t)]
      [(list (list var-types ...) ret-type)
       (proctype (map (lambda (sub-type)
                        (replace1 sub-type sym new-type))
                      var-types)
                 (replace1 ret-type sym new-type))])))

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
(define degrade
  (lambda (equation sub-equa)
    ; (Equation Equation) -> (Equations)
    (match equation
      [(list left1 right1)
       (match sub-equa
         [(list left2 right2)
          (match-type right1 right2)])])))

(define solve-acc
  (lambda (equas normalized-equas subs)
    ; (Equations * Substitions) -> Substitions
    ; accumulator of solve
    (cond ((and (null? equas)
                (null? normalized-equas)) subs)
          ((null? normalized-equas)
           ; create more normalized-equations
           (solve-acc (cdr equas) (normalize-equa (car equas)) subs))
          (else
            (match (equa->sub (car normalized-equas) subs)
             [(list degraded-equas new-subs)
              (solve-acc (append degraded-equas equas)
                         (cdr normalized-equas)
                         new-subs)])))))

(define (solve equas)
  (print-equations equas)
  (print-subs (solve-acc equas '() '())))

(module+ test
  (require rackunit)
  (check-true (occurs? 'a (proctype '(a) 'int)))
  (check-false (occurs? 'a (proctype '(b) 'int)))
  (check-equal? (normalize-equa (equa 'int 'ta))
                (list (equa 'ta 'int)))
  (check-equal? (normalize-equa (equa 'int 'int))
               '())
  (check-equal? (normalize-equa (equa (proctype '(int) 'tb)
                                      (proctype '(ta) 'int)))
               (list (equa 'ta 'int)
                     (equa 'tb 'int)))
  (check-equal? (replace1 (proctype '(a) 'int) 'a (proctype '(b) 'int))
                (proctype (list (proctype '(b) 'int)) 'int))
  (check-equal? (match-type (proctype '(int) 'int)
                            (proctype '(a) 'b))
                '((int a) (int b)))
  (check-equal? (match-type 'int 'int)
                '((int int)))
  (check-equal? (replace1 (proctype '(a) 'int) 'b (proctype '(b) 'int))
                (proctype '(a) 'int))
  )


(module+ test
  (real-typeof (parse '(lambda (v)
                           (zero? v))))
  (real-typeof (parse '(lambda (f)
                           (f 1))))

  (real-typeof (parse '(lambda (f)
                         (lambda (x)
                           (- (f 3) (f x))))))
  )
