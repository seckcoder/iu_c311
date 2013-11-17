; interpreter of let-lang

#lang eopl

(require "../base/utils.rkt")
(require racket/file)

(provide (all-defined))

; expval := Int | Bool | Proc
; during the implemention, I find this datatype actually
; useless...
(define-datatype
  expval expval?
  (numval
    (int integer?))
  (boolval
    (bool boolean?))
  (procval
    (var symbol?)
    (body anything?)))

(define-datatype
  proc proc?
  (closure
    (vars (list-of symbol?))
    (body anything?) 
    (env anything?)))

(define apply-proc
  (lambda (proc1 args)
    (cases
      proc proc1
      (closure
        (vars body env)
        (let ((new-env (extend-env vars
                                   args
                                   env)))
          (interp-exp body new-env)))
      (else (eopl:error 'apply-proc "invalid procedure value:" proc1)))))

; environment

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (vars (list-of symbol?))
    (vals (list-of anything?))
    (env environment?))
  (extend-rec-env
    (pnames (list-of symbol?))
    (b-vars (list-of (list-of symbol?)))
    (b-bodys (list-of anything?))
    (env environment?))
  )

(define apply-env
  (lambda (env search-var)
    (cases
      environment env
      (empty-env
        ()
        (eopl:error 'apply-env "var:~s not found" search-var))
      (extend-env
        (vars vals inherited-env)
        (let ((idx (index-of vars search-var)))
          (if (< idx 0)
            (apply-env inherited-env search-var)
            (list-ref vals idx))))
      (extend-rec-env
        (pnames b-lst-of-vars b-bodies inherited-env)
        (let ((idx (index-of pnames search-var)))
          (if (< idx 0)
            (apply-env inherited-env search-var)
            (closure (list-ref b-lst-of-vars idx)
                     (list-ref b-bodies idx)
                     env))))
        )))

; grammer
(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))


(define grammar-al
  '((program
      (expression)
      a-program)
    (expression
      (number)
      const-exp)
    (expression
      ("-(" expression "," expression ")")
      diff-exp)
    (expression
      ("zero?" "(" expression ")")
      zero?-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      (identifier)
      var-exp)
    (expression
      ("let" (arbno identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (arbno identifier) ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      call-exp)
    (expression
      ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression)
      letrec-exp)
    (expression
      ("{" (arbno expression ";") "}")
      compound-exp)
    ))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))

(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))


(define interp-exps-ret-last
  (lambda (exps env)
    (cond ((null? exps)
           (eopl:error 'compund-exps "no expression in block"))
          ((null? (cdr exps))
           (interp-exp (car exps) env))
          (else
            (interp-exp (car exps) env)
            (interp-exps-ret-last (cdr exps) env)))))

(define interp-exp 
  (lambda (exp env)
    (cases
      expression exp
      (const-exp
        (num)
        num)
      (diff-exp
        (exp1 exp2)
        (- (interp-exp exp1 env)
           (interp-exp exp2 env)))
      (zero?-exp
        (exp)
        (zero? (interp-exp exp env)))
      (if-exp
        (predicate sbj-exp else-exp)
        (if (interp-exp predicate env)
          (interp-exp sbj-exp env)
          (interp-exp else-exp env)))
      (var-exp
        (var)
        (apply-env env var))
      (let-exp
        (vars val-exps exp2)
        (let ((vals (map (lambda (val-exp)
                           (interp-exp val-exp env))
                         val-exps)))
          (let ((new-env (extend-env vars
                                     vals
                                     env)))
            (interp-exp exp2 new-env))))
      (proc-exp
        (vars body)
        (closure vars body env))
      (call-exp
        (proc-exp arg-exps)
        (let ((proc (interp-exp proc-exp env))
              (args (map (lambda (arg-exp)
                           (interp-exp arg-exp env))
                         arg-exps)))
          (apply-proc proc args)))
      (letrec-exp
        (p-names b-lst-of-vars b-bodys letrec-body)
        (let ((new-env (extend-rec-env p-names
                                       b-lst-of-vars
                                       b-bodys
                                       env)))
          (interp-exp letrec-body new-env)))
      (compound-exp
        (exps)
        (interp-exps-ret-last exps env))
      )))

(define initial-env (empty-env))
(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (interp-exp exp initial-env)))))

(define test-prog
  (lambda (prog)
    (display (interp (scan&parse prog)))
    (newline)))

(define test-prog-eqv
  (lambda (prog v)
    (let ((interp-v (interp (scan&parse prog))))
      (if (not (eq? interp-v v))
        (begin
        (display interp-v)(newline)
        (eopl:error 'test-prog-eqv "value-of:~s is ~s not eq ~s" prog interp-v v)
        )
        'ok))))

(define (test)
  (test-prog-eqv "let f = proc (x) -(x,11)
                 in (f (f 77))"
                 55)
  (test-prog-eqv "(proc (f) (f (f 77))
                  proc (x) -(x,11))"
                 55)
  ; the following example is used to identify scoping of the proc-lang.
  ; if it's dynamic scoping, then the result will be 1, else result will be
  ; 2 with lexical scoping
  (test-prog-eqv "let f = let x = 3
                          in proc (y) -(y,x)
                  in let x = 4
                     in (f 5)"
                  2)
  (test-prog-eqv "letrec double(n) = if zero?(n) then 0 else -((double -(n,1)), -2)
                  in (double 6)"
                  12)
  (test-prog-eqv "letrec
                    even(x) = if zero?(x) then 0 else (odd -(x,1))
                    odd(x) = if zero?(x) then 1 else (even -(x,1))
                  in (odd 13)"
                  0)
  (test-prog-eqv "letrec
                    foo(a) = (bar a)
                    bar(b) = b
                  in (foo 3)"
                  3)

  ; for multi args test
  (test-prog-eqv "let f = proc (x y) -(x,y)
                  in (f 10 2)"
                  8)
  (test-prog-eqv "let x = 10
                      y = 2
                      f = proc (x y) -(x,y)
                  in (f x y)"
                  8)
  (test-prog-eqv "letrec
                    foo() = (bar)
                    bar() = -(10,2)
                  in (foo)"
                  8)
  ; test block
  (test-prog-eqv "{
                    -(3,2);
                    -(4,2);
                    -(5,1);
                  }"
                 4)
  (display "finished test...")
  )

; (display (list-the-datatypes))
