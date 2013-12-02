; interpreter of let-lang

#lang eopl

(require racket/file)
(require "../base/utils.rkt")
(require "store1.rkt")

(provide (all-defined-out))

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
  (lambda (proc1 arg-vals arg-outer-refs)
    (cases
      proc proc1
      (closure
        (vars body env)
        (let* ((arg-inner-refs (newrefs arg-vals))
               (new-env (extend-env vars
                                     arg-inner-refs
                                     env)))
          (let ((body-ret (interp-exp body new-env)))
            ; copy back
            (for-each (lambda (inner-ref outer-ref)
                        (setref! outer-ref
                                 (deref inner-ref)))
                      arg-inner-refs
                      arg-outer-refs)
            body-ret)))
      (else (eopl:error 'apply-proc "invalid procedure value:" proc1)))))

; environment

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (vars (list-of symbol?))
    (refs (list-of reference?))
    (env environment?))
  )

(define extend-env-recursively
  (lambda (pnames b-lst-of-vars b-bodies inherited-env)
    (let* ((refs (newrefs (map (lambda (_)
                                '()) pnames)))
           (new-env (extend-env pnames refs inherited-env)))
      (for-each (lambda (ref b-vars b-body)
                  (setref! ref
                          (closure b-vars
                                   b-body
                                   new-env)))
                refs
                b-lst-of-vars
                b-bodies)
      new-env)))

(define apply-env
  (lambda (env search-var)
    (cases
      environment env
      (empty-env
        ()
        (eopl:error 'apply-env "var:~s not found" search-var))
      (extend-env
        (vars refs inherited-env)
        (let ((idx (index-of vars search-var)))
          (if (< idx 0)
            (apply-env inherited-env search-var)
            (list-ref refs idx))))
        )))

; grammar
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
    (expression
      ("set" identifier "=" expression)
      set-exp)
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

(define interp-rand-exp
  (lambda (rand-exp env)
    (cases expression rand-exp
      (var-exp
        (var)
        var)
      (else
        (eopl:error 'interp-rand-exp "procedure parameter must be variables")))))

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
        (deref (apply-env env var)))
      (let-exp
        (vars val-exps exp2)
        (let* ((vals (map (lambda (val-exp)
                           (interp-exp val-exp env))
                         val-exps))
               (new-env (extend-env vars
                                    (newrefs vals)
                                    env)))
          (interp-exp exp2 new-env)))
      (proc-exp
        (vars body)
        (closure vars body env))
      (call-exp
        (proc-exp rand-exps)
        (let* ((proc (interp-exp proc-exp env))
               (rand-vars (map (lambda (rand-exp)
                                 (interp-rand-exp rand-exp env))
                               rand-exps))
               (rand-refs (map (lambda (var)
                                 (apply-env env var))
                               rand-vars))
               (rand-vals (map (lambda (ref)
                                 (deref ref))
                               rand-refs)))
          (apply-proc proc rand-vals rand-refs)))
      (letrec-exp
        (p-names b-lst-of-vars b-bodies letrec-body)
        (let ((new-env (extend-env-recursively p-names
                                               b-lst-of-vars
                                               b-bodies
                                               env)))
          (interp-exp letrec-body new-env)))
      (compound-exp
        (exps)
        (interp-exps-ret-last exps env))
      (set-exp
        (var exp)
        (setref! (apply-env env var)
                 (interp-exp exp env)))
      )))

(define initial-env (empty-env))
(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (initialize-store!)
        (interp-exp exp initial-env)))))

(define test-prog
  (lambda (prog)
    (display (interp (scan&parse prog)))
    (newline)))

(define test-prog-eqv
  (lambda (prog v)
    (let ((interp-v (interp (scan&parse prog))))
      (if (not (eq? interp-v v))
        (eopl:error 'test-prog-eqv "value-of:~s is ~s not eq ~s" prog interp-v v)
        'ok))))

(define (test)
  (test-prog-eqv "let f = proc (x) -(x,11)
                  in let var1 = 77
                     in let var2 = (f var1)
                        in (f var2)"
                 55)
  ; the following example is used to identify scoping of the proc-lang.
  ; if it's dynamic scoping, then the result will be 1, else result will be
  ; 2 with lexical scoping
  (test-prog-eqv "let f = let x = 3
                          in proc (y) -(y,x)
                  in let x = 4
                         y = 5
                     in (f y)"
                  2)
  
  (test-prog-eqv "letrec
                    foo(a) = (bar a)
                    bar(b) = b
                  in let v = 3
                     in (foo v)"
                  3)

  ; for multi args test
  (test-prog-eqv "let f = proc (x y) -(x,y)
                      x = 10
                      y = 2
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
                            
  ; test implicit reference
  (test-prog-eqv "let x = 0
                  in letrec even()
                            = if zero?(x)
                              then 1
                              else {
                                set x = -(x,1);
                                (odd);
                              }
                            odd()
                            = if zero?(x)
                              then 0
                              else {
                                set x = -(x,1);
                                (even);
                              }
                   in {
                    set x = 12; (odd);
                   }"
                   0)
  (test-prog-eqv "let p = proc(x) set x = 4
                  in let a = 3
                     in {
                      (p a);
                      a;
                  }"
                  4) ; call by value result
  ; eopl 4.37 
  ; for call-by-value-result, result is 4
  ; for call-by-reference, result is 3
  (test-prog-eqv "let p = proc(x y) {
                           set y = 4;
                           set x = 3;
                         }
                      x = 0
                  in {
                    (p x x);
                    x;
                  }"
                  4)
  (display "finished test...")
  )
