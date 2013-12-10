; A demo for module system in racket
; run raco test parser.rkt
(module parser racket
  (provide foo)
  (module ds racket
    (require eopl/datatype)
    (require "base/utils.rkt")
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
      (cps-compound-exp
        (exps (list-of simple-exp?))
        (exp tfexp?))
      (cps-letrec-exp
        (p-names (list-of symbol?))
        (procs (list-of simple-exp?))
        (body tfexp?))
      (cps-call-exp
        (rand simple-exp?)
        (rators (list-of simple-exp?)))
      )
    )

  (module in racket
    (provide parse)
    (provide unparse)
    (require (submod ".." ds))
    (define (parse sexp)
      'a)
    (define (unparse exp)
      'b)
    )

  (require (rename-in 'in
                      [parse parse-in]
                      [unparse unparse-in]))

  (module+ test
    (require rackunit)
    (printf "test1\n")
    (check-eq? (unparse-in (parse-in 'a)) 'b))

  (define (foo)
    (print "foo"))
  (module out racket
    (provide parse)
    (provide unparse)
    ; sub module cann't require enclosing module
    #|(require (submod ".."))
    (foo)|#

    ; but it can require non-enclosing module
    (require (submod ".." ds))
    (define (parse sexp)
      'a)
    (define (unparse exp)
      'a)
    (module+ test
      (print "out"))
    )
  (module* barmodule racket
    ; sub module* can require enclosing module
    (require (submod ".."))
    (foo))

  ; enclosing module can require sub module
  (require (rename-in 'out
                      [parse parse-out]
                      [unparse unparse-out]))

  ; but it can't require sub module*
  ; (require 'barmodule)

  (module+ test
    (require (submod ".." out))
    (printf "test2\n")
    (check-eq? (unparse-out (parse-out 'a)) 'a))
  )
