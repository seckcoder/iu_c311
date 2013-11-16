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
    (body anything?) 
    (env anything?)))

(define apply-proc
  (lambda (proc1 arg)
    (cases
      proc proc1
      (closure
        (body env)
        (let ((new-env (extend-dynamic-env arg env)))
          (interp-translated-exp body new-env)))
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

(define apply-dynamic-env
  (lambda (var env)
    (list-ref env var)))
(define extend-dynamic-env
  (lambda (var env)
    (cons var env)))

(define-datatype
  static-env static-env?
  (empty-senv)
  (extend-senv
    (var symbol?)
    (env static-env?))
  (extend-recur-senv
    (proc-name symbol?)
    (proc-body anything?)
    (env static-env?))
  )

(define apply-senv
  (lambda (search-var senv)
    (define iter-senv
      (lambda (search-var senv depth)
        (cases
          static-env senv
          (empty-senv
            ()
            (eopl:error 'apply-senv "var:~s not found" search-var))
          (extend-senv
            (var inherited-env)
            (if (eq? var search-var)
              depth
              (iter-senv search-var inherited-env (+ depth 1))))
          )))
    (iter-senv search-var senv 0)))

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
      ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
      letrec-exp)
    (expression
      ("proc" "(" identifier ")" expression)
      proc-exp)
    (expression
      ("(" expression expression ")")
      call-exp)
    (expression
      ("%lexref" number)
      nameless-var-exp)
    (expression
      ("%let" expression "in" expression)
      nameless-let-exp)
    (expression
      ("%lexproc" expression)
      nameless-proc-exp)
    (expression
      ("%letrec" expression)
      nameless-letrec-exp)
    ))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))

(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))


(define interp-translated-exp
  (lambda (exp env)
    (cases
      expression exp
      (const-exp
        (num)
        num)
      (diff-exp
        (exp1 exp2)
        (- (interp-translated-exp exp1 env)
           (interp-translated-exp exp2 env)))
      (zero?-exp
        (exp)
        (zero? (interp-translated-exp exp env)))
      (if-exp
        (predicate sbj-exp else-exp)
        (if (interp-translated-exp predicate env)
          (interp-translated-exp sbj-exp env)
          (interp-translated-exp else-exp env)))
      (var-exp
        (var)
        (eopl:error 'interp-translated-exp "var-exp in translated program"))
      (let-exp
        (var exp1 exp2)
        (eopl:error 'interp-translated-exp "let-exp in translated program"))
      (proc-exp
        (var body)
        (eopl:error 'interp-translated-exp "proc-exp in translated program"))
      (call-exp
        (exp1 exp2)
        (let ((proc (interp-translated-exp exp1 env))
              (arg (interp-translated-exp exp2 env)))
          (apply-proc proc arg)))
      (nameless-var-exp
        (num)
        (apply-dynamic-env num env))
      (nameless-let-exp
        (exp1 exp2)
        (let ((new-env (extend-dynamic-env (interp-translated-exp exp1 env)
                                           env)))
          (interp-translated-exp exp2 new-env)))
      (nameless-proc-exp
        (exp)
        (closure exp env))
      (nameless-letrec-exp
        (body)
        (interp-translated-exp body new-env))
      )))

(define initial-senv (empty-senv))
(define translate-program
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (a-program (translate-exp exp initial-senv))))))

(define translate-exp
  (lambda (exp senv)
    (define report-error
      (lambda (exp-name)
        (eopl:error 'translate-exp "cannot handle exp:~s in translate-exp" exp-name)))
    (cases
      expression exp
      (const-exp
        (num)
        (const-exp num))
      (diff-exp
        (exp1 exp2)
        (diff-exp (translate-exp exp1 senv)
                  (translate-exp exp2 senv)))
      (zero?-exp
        (exp)
        (zero?-exp (translate-exp exp senv)))
      (if-exp
        (predicate sbj-exp else-exp)
        (if-exp (translate-exp predicate senv)
                (translate-exp sbj-exp senv)
                (translate-exp else-exp senv)))
      (var-exp
        (var)
        (nameless-var-exp (apply-senv var senv)))
      (let-exp
        (var exp1 exp2)
        (let ((new-senv (extend-senv var senv)))
          (nameless-let-exp (translate-exp exp1 senv)
                            (translate-exp exp2 new-senv))))
      (letrec-exp
        (p-name p-var p-body let-body)
        (let ((new-senv1 (extend-senv p-var senv)))
          (let ((new-senv2 (extend-recur-senv p-name p-body new-senv1)))
            (translate-exp p-body new-senv2)

      (proc-exp
        (var body)
        (let ((new-senv (extend-senv var senv)))
          (nameless-proc-exp (translate-exp body new-senv))))
      (call-exp
        (exp1 exp2)
        (call-exp (translate-exp exp1 senv)
                  (translate-exp exp2 senv)))
      (nameless-var-exp
        (num)
        (report-error 'nameless-var-exp))
      (nameless-let-exp
        (exp1 exp2)
        (report-error 'nameless-let-exp))
      (nameless-proc-exp
        (exp)
        (report-error 'nameless-proc-exp))
      (nameless-letrec-exp
        (exp)
        (report-error 'nameless-letrec-exp))
      )))

(define initial-denv '())
(define interp-translated-program
  (lambda (translated-prog)
    (cases
      program translated-prog
      (a-program
        (translated-exp)
        (interp-translated-exp translated-exp initial-denv)))))

(define interp
  (lambda (datum)
    (interp-translated-program (translate-program datum))))

(define (test)
  (define (test-prog prog)
    (display (interp (scan&parse prog)))(newline))
  (test-prog "let f = proc (x) -(x,11)
              in (f (f 77))")
  (test-prog "(proc (f) (f (f 77))
               proc (x) -(x,11))")
  ; the following example is used to identify scoping of the proc-lang.
  ; if it's dynamic scoping, then the result will be 1, else result will be
  ; 2 with lexical scoping
  (test-prog "let f = let x = 3
                      in proc (y) -(y,x)
              in let x = 4
              in (f 5)")
  #|(let ((datum (scan&parse "letrec double(n) = if zero?(n) then 0 else -((double -(n,1)), -2)
                            in (double 6)")))
    (display (interp datum)))|#
  )

; (display (list-the-datatypes))

(test)
