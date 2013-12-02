; interpreter of let-lang

#lang eopl

(require racket/file)
(require racket/match)
(require "../base/utils.rkt")
(require "store.rkt")

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
  answer answer?
  (ans
    (val anything?)
    (store store?)))

(define rs->ans
  (lambda (refstore-val)
    (cases refstore refstore-val
      (rs
        (ref store)
        (ans ref store)))))

(define error-not-ans
  (lambda (sym ans-val)
    (eopl:error sym "~s is not an answer" ans-val)))

(define-datatype
  proc proc?
  (closure
    (vars (list-of symbol?))
    (body anything?) 
    (env anything?)))

(define apply-proc
  (lambda (proc1 args store)
    (cases proc proc1
      (closure
        (vars body env)
        (let ((new-env (extend-env vars
                                   args
                                   env)))
          (interp-exp body new-env store))))))

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
    (cases environment env
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
    (expression
      ("newref" "(" expression ")")
      newref-exp)
    (expression
      ("deref" "(" expression ")")
      deref-exp)
    (expression
      ("setref" "(" expression "," expression ")")
      setref-exp)
    ))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))

(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))

(define interp-exps-ret-last
  (lambda (exps env store)
    (cond ((null? exps)
           (eopl:error 'compund-exps "no expression in block"))
          ((null? (cdr exps))
           (interp-exp (car exps) env store))
          (else
            (cases answer (interp-exp (car exps) env store)
              (ans
                (cur-exp-val new-store)
                (interp-exps-ret-last (cdr exps) env new-store)))))))

(define interp-exps-ret-all
  (lambda (exps env store)
    (if (null? exps)
      (eopl:error 'compund-exps "no expression in block")
      (foldl (lambda (cur-exp accum)
              (cases answer accum
                (ans
                  (vals store)
                  (cases answer (interp-exp cur-exp env store)
                    (ans
                      (cur-val new-store)
                      (ans (append vals (list cur-val))
                           new-store))))))
             (ans '() store)
             exps))))

(define interp-exp 
  (lambda (exp env store)
    (cases expression exp
      (const-exp
        (num)
        (ans num store))
      (diff-exp
        (exp1 exp2)
        (cases answer (interp-exp exp1 env store)
          (ans
            (exp1-val store1)
            (cases answer (interp-exp exp2 env store1)
              (ans
                (exp2-val store2)
                (ans (- exp1-val exp2-val)
                    store2))))))
      (zero?-exp
        (exp)
        (cases answer (interp-exp exp env store)
          (ans
            (exp-val new-store)
            (ans (zero? exp-val) new-store))))
      (if-exp
        (predicate sbj-exp else-exp)
        (cases answer (interp-exp predicate env store)
          (ans
            (exp-val new-store)
            (if exp-val
              (interp-exp sbj-exp env new-store)
              (interp-exp else-exp env new-store)))))
      (var-exp
        (var)
        (ans (apply-env env var)
             store))
      (let-exp
        (vars val-exps exp2)
        (cases answer (interp-exps-ret-all val-exps env store)
          (ans
            (vals new-store)
            (let ((new-env (extend-env vars
                                       vals
                                       env)))
              (interp-exp exp2 new-env new-store)))))
      (proc-exp
        (vars body)
        (ans (closure vars body env) store))
      (call-exp
        (proc-exp arg-exps)
        (cases answer (interp-exps-ret-all (cons proc-exp arg-exps) env store)
          (ans
            (ret new-store)
            (match ret
              [(list proc args ...)
               (apply-proc proc args new-store)]))))
      (letrec-exp
        (p-names b-lst-of-vars b-bodys letrec-body)
        (let ((new-env (extend-rec-env p-names
                                       b-lst-of-vars
                                       b-bodys
                                       env)))
          (interp-exp letrec-body new-env store)))
      (compound-exp
        (exps)
        (interp-exps-ret-last exps env store))
      (newref-exp
        (exp)
        (cases answer (interp-exp exp env store)
          (ans
            (val store1)
            (rs->ans (newref val store1)))))
      (setref-exp
        (exp-ref exp-value)
        (cases answer (interp-exps-ret-all (list exp-ref exp-value) env store)
          (ans
            (ret new-store)
            (match ret
              [(list ref val)
               (ans val (setref ref val new-store))]))))
      (deref-exp
        (exp)
        (cases answer (interp-exp exp env store)
          (ans
            (ref new-store)
            (ans (deref ref new-store) new-store))))
      )))

(define initial-env (empty-env))
(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (cases answer (interp-exp exp initial-env (empty-store))
          (ans
            (result store)
            result))))))

(define test-prog-eqv
  (lambda (prog v)
    (let ((interp-v (interp (scan&parse prog))))
      (if (not (eq? interp-v v))
        (eopl:error 'test-prog-eqv "value-of:~s is ~s not eq ~s" prog interp-v v)
        (println "test of ~s pass..." prog)))))

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

  (test-prog-eqv "let x = newref(0)
                  in letrec even() = if zero?(deref(x))
                                     then 1
                                     else {
                                        setref(x, -(deref(x), 1));
                                        (odd);
                                     }
                            odd() = if zero?(deref(x))
                                    then 0
                                    else {
                                      setref(x, -(deref(x), 1));
                                      (even);
                                    }
                      in {
                        setref(x, 12);
                        (odd);
                      }"
                    0)
                            
  ; test store
  (display "finished test...")
  )
