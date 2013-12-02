; interpreter of let-lang

#lang eopl

(require racket/file)
(require "../base/utils.rkt")
(require "store1.rkt")

(provide (all-defined-out))

(define use-memoization #t)
; expval := Int | Bool | Proc
(define-datatype
  expval expval?
  (numval
    (int integer?))
  (boolval
    (bool boolean?))
  (procval
    (proc proc?))
  (lazyval
    (thunk thunk?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (numval
        (num)
        num)
      (else
        (report-export-error 'numval val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (boolval
        (bool)
        bool)
      (else
        (report-export-error 'boolval val)))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (procval
        (procedure)
        procedure)
      (else
        (report-export-error 'procval val)))))

(define-datatype
  thunk thunk?
  (a-thunk
    (exp anything?)
    (env environment?)))

(define report-export-error
  (lambda (val-name val)
    (eopl:error 'expval-error "expval:~s is not a ~s" val val-name)))

(define-datatype
  proc proc?
  (closure
    (vars (list-of symbol?))
    (body anything?) 
    (env environment?)))

(define-datatype
  argenv argenv?
  (arg-closure
    (argexp anything?)
    (env environment?)))

(define apply-proc
  (lambda (proc-exp-val args)
    (cases proc (expval->proc proc-exp-val)
      (closure
        (vars body env)
        (let ((new-env (extend-env vars
                                   (newrefs args)
                                   env)))
          (interp-exp body new-env))))))

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
                          (procval (closure b-vars
                                            b-body
                                            new-env))))
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

(define interp-exp 
  (lambda (exp env)
    (cases
      expression exp
      (const-exp
        (num)
        (numval num))
      (diff-exp
        (exp1 exp2)
        (let ((num1 (expval->num (interp-exp exp1 env)))
              (num2 (expval->num (interp-exp exp2 env))))
          (numval (- num1 num2))))
      (zero?-exp
        (exp)
        (let ((num (expval->num (interp-exp exp env))))
          (boolval (zero? num))))
      (if-exp
        (predicate sbj-exp else-exp)
        (let ((predv (expval->bool (interp-exp predicate env))))
          (if predv
            (interp-exp sbj-exp env)
            (interp-exp else-exp env))))
      (var-exp
        (var)
        (let* ((ref (apply-env env var))
               (val (deref ref)))
          (cases expval val
            (lazyval
              (tk)
              (cases thunk tk
                (a-thunk
                  (exp env)
                  (let ((thunk-v (interp-exp exp env)))
                    (if use-memoization
                      (setref! ref thunk-v)
                      'ok)
                    thunk-v))))
            (else
              val))))
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
        (procval (closure vars body env)))
      (call-exp
        (proc-exp arg-exps)
        (let ((proc (interp-exp proc-exp env))
              (lazy-args (map (lambda (arg-exp)
                                (lazyval (a-thunk arg-exp env)))
                              arg-exps)))
          (apply-proc proc lazy-args)))
      (letrec-exp
        (p-names b-lst-of-vars b-bodys letrec-body)
        (let ((new-env (extend-env-recursively p-names
                                               b-lst-of-vars
                                               b-bodys
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
    (let ((interp-v (expval->num (interp (scan&parse prog)))))
      (if (not (eq? interp-v v))
        (eopl:error 'test-prog-eqv "value-of:~s is ~s not eq ~s" prog interp-v v)
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
                  3) ; call by value
  
  ; lazy evaluation test
  ; Shit! sllgen doesn't support - and _
  (test-prog-eqv "letrec infiniteloop(x) = (infiniteloop -(x,-1))
                  in let f = proc(x) 11
                     in (f (infiniteloop 2))"
                 11)

  ; why memoization should not be used in
  ; program having side-effect. 
  ; enable `use-memoization` then see the result
  (test-prog-eqv "let a = 2
                      b = 3
                  in let f = proc(x) {
                              x;
                              set a = 4;
                              x;
                             }
                     in (f -(a,b)) "
                  -1)
  ; for 4.38. the modified version can't run 
  ; under call-by-value. Reason is not lazy enough,
  ; as a result, infinite-recursion.
  (test-prog-eqv "let makerec = proc (f)
                                  let d = proc (x)
                                            proc (z) ((f (x x)) z)
                                  in proc (n) ((f (d d)) n)
                  in let maketimes4 = proc (f)
                                        proc (x)
                                          if zero?(x)
                                          then 0
                                          else -((f -(x,1)), -4)
                     in let times4 = (makerec maketimes4) in (times4 3)"
                   12)
  (test-prog-eqv "let makrec = proc(f)
                                let d = proc (x) (f (x x))
                                in (f (d d))
                  in let maketimes4 = proc(f)
                                        proc(x)
                                          if zero?(x)
                                          then 0
                                          else -((f -(x,1)), -4)
                     in let times4 = (makrec maketimes4)
                        in (times4 3)"
                  12)
  (display "finished test...")
  )
