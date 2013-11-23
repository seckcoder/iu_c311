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
    (var symbol?)
    (body anything?) 
    (env anything?)))

(define apply-proc
  (lambda (proc1 arg cont)
    (cases
      proc proc1
      (closure
        (var body env)
        (let ((new-env (extend-env var
                                   arg
                                   env)))
          (interp-exp/k body new-env cont)))
      (else (eopl:error 'apply-proc "invalid procedure value:" proc1)))))

; environment

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (val anything?)
    (env environment?))
  (extend-rec-env
    (pname symbol?)
    (b-var symbol?)
    (b-body anything?)
    (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases
      environment env
      (empty-env
        ()
        (eopl:error 'apply-env "var:~s not found" search-var))
      (extend-env
        (var val inherited-env)
        (if (eq? var search-var)
          val
          (apply-env inherited-env search-var)))
      (extend-rec-env
        (pname b-var b-body inherited-env)
        (if (eq? search-var pname)
          (closure b-var b-body env)
          (apply-env inherited-env search-var)))
        )))


(define-datatype
  continuation continuation?
  (end-cont)
  (zero-cont
    (cont continuation?))
  (let-cont
    (var symbol?)
    (env environment?)
    (body anything?)
    (cont continuation?))
  (diff-subtractor-cont
    (subtractor-exp anything?)
    (env environment?)
    (cont continuation?))
  (diff-cont
    ; todo (expval support)
    (minuend-val anything?)
    (env environment?)
    (cont continuation?))
  (if-cont
    (sbj-exp anything?)
    (else-exp anything?)
    (env environment?)
    (cont continuation?))
  (call-exp-arg-cont
    (arg-exp anything?)
    (env environment?)
    (cont continuation?))
  (call-exp-cont
    (proc proc?)
    (env environment?)
    (cont continuation?))
  )

(define apply-cont
  (lambda (cont exp-val)
    (cases continuation cont
      (end-cont
        ()
        (println "End of computation")
        exp-val)
      (zero-cont
        (next-cont)
        (apply-cont next-cont (zero? exp-val)))
      (let-cont
        (var env body next-cont)
        (let ((new-env (extend-env var
                                   exp-val
                                   env)))
          (interp-exp/k body new-env next-cont)))
      (diff-subtractor-cont
        (subtractor-exp env next-cont)
        (interp-exp/k subtractor-exp env
                      (diff-cont exp-val env next-cont)))
      (diff-cont
        (minuend-val env next-cont)
        (apply-cont next-cont (- minuend-val exp-val)))
      (if-cont
        (sbj-exp else-exp env next-cont)
        (if exp-val
          (interp-exp/k sbj-exp env next-cont)
          (interp-exp/k else-exp env next-cont)))
      (call-exp-arg-cont
        (arg-exp env next-cont)
        (interp-exp/k arg-exp env (call-exp-cont exp-val env next-cont)))
      (call-exp-cont
        (proc env next-cont)
        (apply-proc proc exp-val next-cont))
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
      ("let" identifier "=" expression "in" expression)
      let-exp)
    (expression
      ("proc" "(" identifier ")" expression)
      proc-exp)
    (expression
      ("(" expression expression ")")
      call-exp)
    (expression
      ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
      letrec-exp)
    ))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))

(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))


(define interp-exp/k
  (lambda (exp env cont)
    (cases
      expression exp
      (const-exp
        (num)
        (apply-cont cont num))
      (diff-exp
        (minuend subtractor)
        (interp-exp/k minuend env (diff-subtractor-cont subtractor env cont)))
      (zero?-exp
        (exp)
        (interp-exp/k exp env (zero-cont cont)))
      (if-exp
        (predicate sbj-exp else-exp)
        (interp-exp/k predicate env (if-cont sbj-exp else-exp env cont)))
      (var-exp
        (var)
        (apply-cont cont (apply-env env var)))
      (let-exp
        (var exp1 body)
        (interp-exp/k exp1 env (let-cont var env body cont)))
      (proc-exp
        (var body)
        (apply-cont cont (closure var body env)))
      (call-exp
        (exp1 exp2)
        (interp-exp/k exp1 env (call-exp-arg-cont exp2 env cont)))
      (letrec-exp
        (p-name b-var b-body letrec-body)
        (let ((new-env (extend-rec-env p-name
                                       b-var
                                       b-body
                                       env)))
          (interp-exp/k letrec-body new-env cont)))
      )))

(define initial-env (empty-env))
(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (interp-exp/k exp initial-env (end-cont))))))


(define test-prog-eqv 
  (lambda (prog v)
    (check eq? (interp (scan&parse prog)) v)))

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
  (test-prog-eqv "letrec double(x) = if zero?(x)
                                     then 0
                                     else -((double -(x,1)), -2)
                  in (double 4)"
                  8)
  )

; (display (list-the-datatypes))

; (test)
