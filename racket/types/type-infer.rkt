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
  (not (proc? t)))

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
  (println subs))

(define (occurs? sym type)
  (match type
    [(? symbol? t) (eq? sym t)]
    [(list (list var-types ...) ret-type)
     (ormap (lambda (t)
               (occurs? sym t))
             (cons ret-type var-types))]))
     
(define (verify-equa equation)
  (match equation
    [(list left right)
      (cond ((and (type-var? left)
                  (type-var? right)
                  (eq? left right))
             (list #f equation)) ; useless equation
            ((and (type-var? left)
                  (type-var? right))
             (list #t equation))
            ((type-var? left)
             (if (occurs? left right)
               (error 'verify-equa "fail occurence check")
               (list #t equation)))
            ((type-var? right)
             (verify-equa (equa right left)))
            (else
              (error 'verify-equa "weired equation:~s" equation)))]))

(define (solve equas)
  (letrec ((equa->sub (lambda (equation subs)
                        ; (Equation * Substitions) -> (Equations Substitions)
                        ; Send an equation to substitions. It's possible that
                        ; the equation is turned to substition. It's also possible
                        ; that the equation is degraded into multi equations.
                        (match (find (lambda (sub-equa)
                                       (eq? (equa->left sub-equa)
                                            (equa->left equation)))
                                     subs)
                          [(list finded? sub-equa rest ...)
                           (if finded?
                             (list (degrade equation sub-equa) subs)
                             (list '() (replace equation subs)))])))
           (replace (lambda (equation subs)
                      ; (Equation Substitions) -> Substitions
                      ; replace every substition and move equation to subs
                      (match equation
                        [(list left1 right1)
                         (append (map (lambda (sub)
                                        (match sub
                                          [(list left2 right2)
                                           (equa left2 (replace1 right2 left1 right1))]))
                                      subs)
                                 (list equation))])))
           (replace1 (lambda (type sym new-type)
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
           (solve-acc (lambda (equas subs)
                        ; (Equations * Substitions) -> Substitions
                        ; accumulator of solve
                        (if (null? equas)
                          subs
                          (match (equa->sub (car equas) subs)
                            [(list degraded-equas new-subs)
                             (solve-acc (append degraded-equas equas)
                                        new-subs)])))))
    (solve-acc equas '())))

(module+ test
  (real-typeof (parse '(lambda (v)
                         (zero? v))))
  (real-typeof (parse '(lambda (f)
                         (f 1))))
  (real-typeof (parse '(lambda (f)
                         (lambda (x)
                           (- (f 3) (f x))))))
  )
