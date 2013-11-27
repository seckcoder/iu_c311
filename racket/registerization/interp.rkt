; interpreter of let-lang

#lang eopl

(require "../base/utils.rkt")
(require "store.rkt")
(require "register.rkt")
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
  (listval
    (list list?))
  (procval
    (proc proc?)))

(define expval->normalval
  (lambda (val)
    (cases expval val
      (numval
        (num)
        num)
      (boolval
        (bool)
        bool)
      (listval
        (lst)
        (map expval->normalval lst))
      (procval
        (proc)
        proc))))

(define expval->listval
  (lambda (val)
    (cases expval val
      (listval
        (lst)
        lst)
      (else
        (eopl:error 'expval->listval "~s is not a list" val)))))

(define expval->numval
  (lambda (val)
    (cases expval val
      (numval
        (num)
        num)
      (else
        (eopl:error 'expval->numval "~s is not a num" val)))))

(define expval->boolval
  (lambda (val)
    (cases expval val
      (boolval
        (bool)
        bool)
      (else
        (eopl:error 'expval->boolval "~s is not a bool" val)))))
(define expval->procval
  (lambda (val)
    (cases expval val
      (procval
        (proc)
        proc)
      (else
        (eopl:error 'expval->procval "~s is not a proc" val)))))

(define-datatype
  proc proc?
  (closure
    (var symbol?)
    (body expression?) 
    (env environment?)))

(define apply-proc
  (lambda ()
    (let ((proc1 (reg-get 'proc))
          (arg (reg-get 'exp-val)))
      (cases
        proc proc1
        (closure
          (var body env)
          (let* ((ref (newref arg))
                 (new-env (extend-env var
                                     ref
                                     env)))
            ; (println "apply-proc arg:~s" arg)
            (reg-set 'exp body)
            (reg-set 'env new-env)
            (interp-exp/k)))
        (else (eopl:error 'apply-proc "invalid procedure value:" proc1))))))

; environment

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (ref reference?)
    (env environment?)))

(define extend-env-recursively
  (lambda (p-name b-var b-body env)
    (let* ((proc-ref (newref '()))
           (new-env (extend-env p-name
                                proc-ref
                                env)))
      (setref! proc-ref (procval (closure b-var
                                          b-body
                                          new-env)))
      new-env)))

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
      )))


(define-datatype
  continuation continuation?
  (end-cont)
  (zero1-cont
    (cont continuation?))
  (let-exp-cont
    (var symbol?)
    (env environment?)
    (body expression?)
    (cont continuation?))
  (diff1-cont
    (subtractor-exp expression?)
    (env environment?)
    (cont continuation?))
  (diff2-cont
    ; todo (expval support)
    (minuend-val expval?)
    (env environment?)
    (cont continuation?))
  (if-test-cont
    (sbj-exp expression?)
    (else-exp expression?)
    (env environment?)
    (cont continuation?))
  (rator-cont
    (arg-exp expression?)
    (env environment?)
    (cont continuation?))
  (rand-cont
    (proc proc?)
    (cont continuation?))
  (cons1-cont
    (cdrv-exp expression?)
    (env environment?)
    (cont continuation?))
  (cons2-cont
    (carv expval?)
    (env environment?)
    (cont continuation?))
  (car-exp-cont
    (cont continuation?))
  (cdr-exp-cont
    (cont continuation?))
  (is-empty-exp-cont
    (cont continuation?))
  (multi-exp-cont
    (exps (list-of expression?))
    (accum-op procedure?)
    (accum expval?)
    (env environment?)
    (cont continuation?))
  (set-rhs-cont
    (ref reference?)
    (env environment?)
    (cont continuation?))
  (mult-cont1
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (mult-cont2
    (val expval?)
    (cont continuation?))
  (print-cont
    (cont continuation?))
  )

(define sigend 0)
(define apply-cont
  (lambda ()
    (let ((cont (reg-get 'cont))
          (exp-val (reg-get 'exp-val))
          (env (reg-get 'env)))
      (cases continuation cont
        (end-cont
          ()
          (println "End of computation")
          (set! sigend (+ sigend 1))
          exp-val)
        (zero1-cont
          (next-cont)
          (reg-set 'cont next-cont)
          (reg-set 'exp-val (boolval (zero? (expval->numval exp-val))))
          (apply-cont))
        (let-exp-cont
          (var env body next-cont)
          (let* ((ref (newref exp-val))
                 (new-env (extend-env var
                                     ref
                                     env)))
            (reg-set 'env new-env)
            (reg-set 'exp body)
            (reg-set 'cont next-cont)
            (interp-exp/k)))
        (diff1-cont
          (subtractor-exp env next-cont)
          (reg-set 'exp subtractor-exp)
          (reg-set 'cont (diff2-cont exp-val env next-cont))
          (interp-exp/k))
        (diff2-cont
          (minuend-val env next-cont)
          (reg-set 'exp-val (numval (- (expval->numval minuend-val)
                                           (expval->numval exp-val))))
          (reg-set 'cont next-cont)
          (apply-cont))
        (if-test-cont
          (sbj-exp else-exp env next-cont)
          (reg-set 'cont next-cont)
          (if (expval->boolval exp-val)
            (begin
              (reg-set 'exp sbj-exp)
              (interp-exp/k))
            (begin
              (reg-set 'exp else-exp)
              (interp-exp/k))))
        (rator-cont
          (arg-exp env next-cont)
          (reg-set 'cont (rand-cont (expval->procval exp-val) next-cont))
          (reg-set 'exp arg-exp)
          (reg-set 'env env)
          (interp-exp/k))
        (rand-cont
          (proc next-cont)
          (reg-set 'cont next-cont)
          (reg-set 'proc proc)
          (reg-set 'exp-val exp-val)
          (apply-proc))
        (cons1-cont
          (cdrv-exp env next-cont)
          (reg-set 'exp cdrv-exp)
          (reg-set 'cont (cons2-cont exp-val env next-cont))
          (interp-exp/k))
        (cons2-cont
          (carv env next-cont)
          (reg-set 'cont next-cont)
          (reg-set 'exp-val (listval (cons carv
                                               (expval->listval exp-val))))
          (apply-cont))
        (car-exp-cont
          (next-cont)
          (reg-set 'exp-val (car (expval->listval exp-val)))
          (reg-set 'cont next-cont)
          (apply-cont))
        (cdr-exp-cont
          (next-cont)
          (reg-set 'cont next-cont)
          (reg-set 'exp-val (cdr (expval->listval exp-val)))
          (apply-cont))
        (is-empty-exp-cont
          (next-cont)
          (reg-set 'cont next-cont)
          (reg-set 'exp-val (boolval (null? (expval->listval exp-val))))
          (apply-cont))
        (multi-exp-cont
          (exps accum-op accum env next-cont)
          (let ((new-accum (accum-op exp-val accum)))
            (if (null? exps)
              (begin
                (reg-set 'exp-val new-accum)
                (reg-set 'cont next-cont)
                (reg-set 'env env)
                (apply-cont))
              (begin
                (reg-set 'exp (car exps))
                (reg-set 'cont (multi-exp-cont (cdr exps)
                                               accum-op
                                               new-accum
                                               env
                                               next-cont))
                (reg-set 'env env)
                (interp-exp/k)))))
        (set-rhs-cont
          (var-ref env next-cont)
          (reg-set 'cont next-cont)
          (reg-set 'env env)
          (reg-set 'exp-val (setref! var-ref exp-val))
          (apply-cont))
        (mult-cont1
          (exp2 env next-cont)
          (reg-set 'exp exp2)
          (reg-set 'cont (mult-cont2 exp-val next-cont))
          (reg-set 'env env)
          (interp-exp/k))
        (mult-cont2
          (val1 next-cont)
          (reg-set 'cont next-cont)
          (reg-set 'exp-val (numval (* (expval->numval val1)
                                           (expval->numval exp-val))))
          (apply-cont))
        (print-cont
          (next-cont)
          (reg-set 'exp-val exp-val)
          (reg-set 'cont next-cont)
          (apply-cont))
        ))))

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
    (expression
      ("cons(" expression "," expression ")")
      cons-exp)
    (expression
      ("emptyList")
      empty-lst-exp)
    (expression
      ("car(" expression ")")
      car-exp)
    (expression
      ("cdr(" expression ")")
      cdr-exp)
    (expression
      ("null?(" expression ")")
      is-empty-exp)
    (expression
      ("list(" (arbno expression) ")")
      list-exp)
    (expression
      ("{" (arbno expression ";") "}")
      compound-exp)
    (expression
      ("set" identifier "=" expression)
      set-exp)
    (expression
      ("*(" expression "," expression ")")
      mult-exp)
    (expression
      ("print" expression)
      print-exp)
    ))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))

(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))

#|(define interp-exps/k
  (lambda (exps accum-op accum env cont)
    (if (null? exps)
      (apply-cont cont accum)
      (interp-exp/k (car exps)
                    env
                    (multi-exp-cont (cdr exps)
                                    accum-op
                                    accum
                                    env
                                    cont)))))|#

(define interp-exp/k
  (lambda ()
    ; (display cont)(newline)(newline)
    (let ((cont (reg-get 'cont))
          (env (reg-get 'env))
          (exp (reg-get 'exp)))
      (cases
        expression exp
        (const-exp
          (num)
          (reg-set 'exp-val (numval num))
          (apply-cont))
        (diff-exp
          (minuend subtractor)
          (reg-set 'cont (diff1-cont subtractor env cont))
          (reg-set 'exp minuend)
          (interp-exp/k))
        (zero?-exp
          (exp)
          (reg-set 'cont (zero1-cont cont))
          (reg-set 'exp exp)
          (interp-exp/k))
        (if-exp
          (predicate sbj-exp else-exp)
          (reg-set 'exp predicate)
          (reg-set 'cont (if-test-cont sbj-exp else-exp env cont))
          (interp-exp/k))
        (var-exp
          (var)
          (reg-set 'exp-val (deref (apply-env env var)))
          (apply-cont))
        (let-exp
          (var exp1 body)
          (reg-set 'exp exp1)
          (reg-set 'cont (let-exp-cont var env body cont))
          (interp-exp/k))
        (proc-exp
          (var body)
          (reg-set 'exp-val (procval (closure var body env)))
          (apply-cont))
        (call-exp
          (exp1 exp2)
          (reg-set 'exp exp1)
          (reg-set 'cont (rator-cont exp2 env cont))
          (interp-exp/k))
        (letrec-exp
          (p-name b-var b-body letrec-body)
          (let ((new-env (extend-env-recursively p-name
                                                 b-var
                                                 b-body
                                                 env)))
            (reg-set 'env new-env)
            (reg-set 'exp letrec-body)
            (interp-exp/k)))
        (cons-exp
          (carv-exp cdrv-exp)
          (reg-set 'exp carv-exp)
          (reg-set 'cont (cons1-cont cdrv-exp env cont))
          (interp-exp/k))
        (car-exp
          (lst-exp)
          (reg-set 'exp lst-exp)
          (reg-set 'cont (car-exp-cont cont))
          (interp-exp/k))
        (cdr-exp
          (lst-exp)
          (reg-set 'exp lst-exp)
          (reg-set 'cont (cdr-exp-cont cont))
          (interp-exp/k))
        (empty-lst-exp
          ()
          (reg-set 'exp-val (listval '()))
          (apply-cont))
        (is-empty-exp
          (lst-exp)
          (reg-set 'exp lst-exp)
          (reg-set 'cont (is-empty-exp-cont cont))
          (interp-exp/k))
        (list-exp
          (exps)
          (if (null? exps)
            (begin
              (reg-set 'exp-val (listval '()))
              (apply-cont))
            (begin
              (reg-set 'exp (car exps))
              (reg-set 'cont (multi-exp-cont (cdr exps)
                                        (lambda (val accum)
                                          (listval (append (expval->listval accum)
                                                           (list val))))
                                        (listval '())
                                        env
                                        cont))
              (interp-exp/k))))
        (compound-exp
          (exps)
          (reg-set 'exp (car exps))
          (reg-set 'cont (multi-exp-cont (cdr exps)
                                         (lambda (val accum)
                                           val)
                                         (listval '())
                                         env
                                         cont))
          (interp-exp/k))
        (set-exp
          (var exp)
          (reg-set 'exp exp)
          (reg-set 'cont (set-rhs-cont (apply-env env var)
                                       env
                                       cont))
          (interp-exp/k))
        (mult-exp
          (exp1 exp2)
          (reg-set 'exp exp1)
          (reg-set 'cont (mult-cont1 exp2 env cont))
          (interp-exp/k))
        (print-exp
          (exp)
          (reg-set 'exp exp)
          (reg-set 'cont (print-cont cont))
          (interp-exp/k))
        ))))

(define initial-env (empty-env))
(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (set! sigend 0)
        (initialize-store!)
        (reg-set 'exp exp)
        (reg-set 'env initial-env)
        (reg-set 'cont (end-cont))
        (let ((result (interp-exp/k)))
          (if (= sigend 1)
            (expval->normalval result)
            (eopl:error 'interp "violate cps")
            ))))))

(define test-prog
  (lambda (prog)
    (interp (scan&parse prog))))

(define test-prog-eqv 
  (lambda (prog v)
    (check eq? (interp (scan&parse prog)) v)))

(define test-prog-equalv
  (lambda (prog v)
    (check equal? (interp (scan&parse prog)) v)))

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
  
  ; test list
  (test-prog-equalv "let x = 4
                     in cons(x, 
                             cons(cons(-(x,1),
                                       emptyList),
                                  emptyList))"
                  '(4 (3)))
  (test-prog-equalv "list()"
                    '())
  (test-prog-equalv "list(1 2 3 list(2 3) 4 5)"
                    '(1 2 3 (2 3) 4 5))

  ; test implicit ref and call by value
  (test-prog-eqv "let p = proc(x) set x = 4
                  in let a = 3
                     in {
                      (p a);
                      a;
                  }"
                  3)

   (test-prog-eqv "let x = 3
                   in {
                      let x = 4
                      in x;
                      x;
                   }"
                   3)
   (test-prog-eqv "let x = 4
                   in let foo = proc (x) x
                      in {
                       (foo let x = 5
                            in x);
                       x;
                      }"
                   4)
   ; fact
   (test-prog-eqv "letrec fact(n) = if zero?(n)
                                    then 1
                                    else *(n, (fact -(n, 1)))
                   in (fact 4)"
                   24)
  )
