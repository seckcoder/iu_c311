; interpreter of let-lang

#lang eopl

(require "../base/utils.rkt")
(require racket/file)

(provide (all-defined))

; expval := Int | Bool
(define-datatype
  expval expval?
  (numval
    (int integer?))
  (boolval
    (bool boolean?)))


; environment

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (val anything?)
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
          (apply-env inherited-env search-var))))))


; grammer
(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))


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
  (let ((datum (scan&parse (file->string "demo.let"))))
    (display (interp datum)))
  )

; (display (list-the-datatypes))

; (test)
