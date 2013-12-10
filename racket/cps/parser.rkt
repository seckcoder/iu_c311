; scheme parser; scheme cps parser and unparser.
(module parser racket
  (provide in:parse
           out:parse
           out:unparse)
  (module ds racket
    (require eopl/datatype)
    (require "../base/utils.rkt")
    (provide (all-defined-out))
    (define op?
      (lambda (op)
        (memq op '(+ - * / = zero? cons car cdr list null? not
                     number? symbol? list? eq? eqv? equal? void
                     atom? void?))))
    (define-datatype
      expression expression?
      (const-exp
        (cst const?))
      (var-exp
        (var symbol?))
      (quote-exp
        (exp sexp?))
      (op-exp
        (op op?)
        (params (list-of expression?)))
      (lambda-exp
        (vars (list-of symbol?))
        (body expression?))
      (if-exp
        (test expression?)
        (then expression?)
        (else expression?))
      (call-exp
        (rand expression?)
        (rators (list-of expression?)))
      (letrec-exp
        (p-names (list-of symbol?))
        (procs (list-of expression?))
        (body expression?))
      )

    (define-datatype
      simple-exp simple-exp?
      (cps-const-exp
        (cst const?))
      (cps-var-exp
        (var symbol?))
      (cps-quote-exp
        (exp sexp?))
      (cps-op-exp
        (op op?)
        (params (list-of simple-exp?)))
      (cps-lambda-exp
        (vars (list-of symbol?))
        (body tfexp?))
      )

    (define-datatype
      tfexp tfexp?
      (simple-exp->exp
        (exp simple-exp?))
      (cps-if-exp
        (test simple-exp?)
        (then tfexp?)
        (else tfexp?))
      (cps-letrec-exp
        (p-names (list-of symbol?))
        (procs (list-of simple-exp?))
        (body tfexp?))
      (cps-call-exp
        (rator simple-exp?)
        (rands (list-of simple-exp?)))
      )
    )

  (module in racket
    (provide parse)
    (provide unparse)
    (require (submod ".." ds))
    (require "../base/utils.rkt")
    (define parse
      (lambda (sexp)
        (match sexp
          [(? const? x) (const-exp x)]
          [(? symbol? x) (var-exp x)]
          [`(quote ,x)
            (quote-exp x)]
          [(list (? op? op) params* ...)
           (op-exp op (map parse params*))]
          [`(lambda (,params ...) ,body)
            (lambda-exp params
                        (parse body))]
          [`(if ,test ,then ,else)
            (if-exp (parse test)
                    (parse then)
                    (parse else))]
          [`(if ,test ,then)
            (parse `(if ,test ,then (void)))]
          [`(letrec ((,name* ,proc*) ...) ,body)
            (letrec-exp name*
                        (map parse proc*)
                        (parse body))]
          [`(let ((,var ,val) ...) ,body)
            (parse `((lambda (,@var)
                       ,body)
                     ,@val))]
          [(list rand rators ...)
           (call-exp (parse rand)
                     (map parse rators))]
          )))
    (define (unparse exp)
      (error "unparser not implemented"))
    )

  (require (prefix-in in: 'in))

  (module out racket
    (provide parse)
    (provide unparse)
    (require eopl/datatype)
    (require (submod ".." ds))
    (require "../base/utils.rkt")
    (define (parse1 sexp)
      (match sexp
        [(? const? x) (cps-const-exp x)]
        [(? symbol? x) (cps-var-exp x)]
        ; quoted sexp
        [`(quote ,x)
          (cps-quote-exp x)]
        ; builtin ops
        [(list (? op? op) params* ...)
         (cps-op-exp op (map parse1-or-fail params*))]
        ; lambda
        [`(lambda (,params ...) ,body)
          (cps-lambda-exp params
                          (parse body))]

        [_ #f]))

    (define (parse1-or-fail sexp)
      (let ((v (parse1 sexp)))
        (if v
          v
          (error 'parse1-or-fail "~s is not a simple expression" sexp))))

    (define (parse sexp)
      (let ((parse1-res (parse1 sexp)))
        (if parse1-res
          (simple-exp->exp parse1-res)
          (match sexp
            ; if 
            [`(if ,test ,then ,else)
              (cps-if-exp (parse1-or-fail test)
                          (parse then)
                          (parse else))]
            [`(if ,test ,then)
              (parse `(if ,test ,then (void)))]
            ; compound exp
            #|[`(begin ,body ,bodies* ...)
              (if (null? bodies*)
                (cps-compound-exp '()
                                  (parse body))
                (cps-compound-exp (map parse1-or-fail 
                                       (cons body (drop-right bodies* 1)))
                                  (parse (take-right bodies* 1))))]|#
            ; let
            [`(let ((,var ,val) ...) ,body)
              (parse `((lambda (,@var)
                         ,body)
                       ,@val))]
            ; letrec
            [`(letrec ((,name* ,proc*) ...) ,body ...)
              (cps-letrec-exp name*
                              (map parse1-or-fail proc*)
                              (parse body))]
            ; procedure call
            [(list rand rators ...)
             (cps-call-exp (parse1-or-fail rand)
                           (map parse1-or-fail rators))]
            ))))
    (define (unparse1 exp)
      (cases simple-exp exp
        (cps-const-exp
          (cst) cst)
        (cps-var-exp
          (var) var)
        (cps-quote-exp
          (exp) `(quote ,exp))
        (cps-op-exp
          (op params)
          `(,op ,@(map unparse1 params)))
        (cps-lambda-exp
          (vars body)
          `(lambda (,@vars)
             ,(unparse body)))
        ))

    (define (unparse exp)
      (cases tfexp exp
        (simple-exp->exp
          (exp) (unparse1 exp))
        (cps-if-exp
          (test then else)
          `(if ,(unparse1 test)
             ,(unparse then)
             ,(unparse else)))
        (cps-letrec-exp
          (p-names procs body)
          `(letrec ,(map list p-names (map unparse1 procs))
             ,(unparse body)))
        (cps-call-exp
          (rand rators)
          `(,(unparse1 rand) ,@(map unparse1 rators)))
        )) 
    )

  (require (prefix-in out: 'out))

  (module+ test
    (require rackunit)
    (require (submod ".." out))
    (define test-out:unparse
      (lambda args
        (match args
          [(list) (void)]
          [(list prog desc rest ...)
           (check-equal? (out:unparse (out:parse prog)) prog desc)
           (apply test-out:unparse rest)])))
    (test-out:unparse 'a "simple variable"
                      ''a "simple symbol"
                      '(foo a) "procedure call"
                      '(if a
                         (foo a)
                         (bar a)) "if exp"
                      '((lambda (v)
                          (foo v))
                        v) "lambda exp")
    )
  )
