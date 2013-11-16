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
  (lambda (proc1 arg)
    (cases
      proc proc1
      (closure
        (var body env)
        (let ((new-env (extend-env var
                                   arg
                                   env)))
          (interp-exp body new-env)))
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
      ("(-" expression expression ")")
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
        (var exp1 exp2)
        (let ((new-env (extend-env var
                                   (interp-exp exp1 env)
                                   env)))
          (interp-exp exp2 new-env)))
      (proc-exp
        (var body)
        (closure var body env))
      (call-exp
        (exp1 exp2)
        (let ((proc (interp-exp exp1 env))
              (arg (interp-exp exp2 env)))
          (apply-proc proc arg)))
      (letrec-exp
        (p-name b-var b-body letrec-body)
        (let ((new-env (extend-rec-env p-name
                                       b-var
                                       b-body
                                       env)))
          (interp-exp letrec-body new-env)))
      )))

(define initial-env (empty-env))
(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (interp-exp exp initial-env)))))

(define (test)
  (let ((datum (scan&parse "let f = proc (x) (- x 11)
                             in (f (f 77))")))
    (display (interp datum))(newline))
  (let ((datum (scan&parse "(proc (f) (f (f 77))
                             proc (x) (- x 11))")))
    (display (interp datum))(newline))
  
  ; the following example is used to identify scoping of the proc-lang.
  ; if it's dynamic scoping, then the result will be 1, else result will be
  ; 2 with lexical scoping
  (let ((datum (scan&parse "let f = let x = 3
                                    in proc (y) (- y x)
                            in let x = 4
                               in (f 5)")))
    (display (interp datum))(newline))

  (let ((datum (scan&parse "letrec double(n) = if zero?(n) then 0 else (- (double (- n 1)) -2)
                            in (double 6)")))
    (display (interp datum)))
  )

; (display (list-the-datatypes))

(test)
