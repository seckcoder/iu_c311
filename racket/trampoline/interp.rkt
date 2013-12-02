; interpreter of let-lang

#lang eopl

(require "../base/utils.rkt")
(require "store.rkt")
(require racket/file)

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
  (lambda (proc1 arg cont)
    (lambda ()
      (cases
        proc proc1
        (closure
          (var body env)
          (let* ((ref (newref arg))
                 (new-env (extend-env var
                                     ref
                                     env)))
            (interp-exp/k body new-env cont)))
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
    (env environment?)
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
  )

(define sigend 0)
(define apply-cont
  (lambda (cont exp-val)
    (cases continuation cont
      (end-cont
        ()
        (println "End of computation")
        (set! sigend (+ sigend 1))
        exp-val)
      (zero1-cont
        (next-cont)
        (apply-cont next-cont (boolval (zero? (expval->numval exp-val)))))
      (let-exp-cont
        (var env body next-cont)
        (let* ((ref (newref exp-val))
               (new-env (extend-env var
                                   ref
                                   env)))
          (interp-exp/k body new-env next-cont)))
      (diff1-cont
        (subtractor-exp env next-cont)
        (interp-exp/k subtractor-exp env
                      (diff2-cont exp-val env next-cont)))
      (diff2-cont
        (minuend-val env next-cont)
        (apply-cont next-cont (numval (- (expval->numval minuend-val)
                                         (expval->numval exp-val)))))
      (if-test-cont
        (sbj-exp else-exp env next-cont)
        (if (expval->boolval exp-val)
          (interp-exp/k sbj-exp env next-cont)
          (interp-exp/k else-exp env next-cont)))
      (rator-cont
        (arg-exp env next-cont)
        (interp-exp/k arg-exp env (rand-cont (expval->procval exp-val) env next-cont)))
      (rand-cont
        (proc env next-cont)
        (apply-proc proc exp-val next-cont))
      (cons1-cont
        (cdrv-exp env next-cont)
        (interp-exp/k cdrv-exp env (cons2-cont exp-val env next-cont)))
      (cons2-cont
        (carv env next-cont)
        (apply-cont next-cont (listval (cons carv
                                             (expval->listval exp-val)))))
      (car-exp-cont
        (next-cont)
        (apply-cont next-cont (car (expval->listval exp-val))))
      (cdr-exp-cont
        (next-cont)
        (apply-cont next-cont (cdr (expval->listval exp-val))))
      (is-empty-exp-cont
        (next-cont)
        (apply-cont next-cont (boolval (null? (expval->listval exp-val)))))
      (multi-exp-cont
        (exps accum-op accum env next-cont)
        (interp-exps/k exps
                       accum-op
                       (accum-op exp-val accum)
                       env
                       next-cont))
      (set-rhs-cont
        (var-ref env next-cont)
        (apply-cont next-cont (setref! var-ref exp-val)))
      (mult-cont1
        (exp2 env next-cont)
        (interp-exp/k exp2 env (mult-cont2 exp-val next-cont)))
      (mult-cont2
        (val1 next-cont)
        (apply-cont next-cont (numval (* (expval->numval val1)
                                         (expval->numval exp-val)))))
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
    ))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-a grammar-al)))

(sllgen:make-define-datatypes scanner-spec-a grammar-al)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-al))

(define interp-exps/k
  (lambda (exps accum-op accum env cont)
    (if (null? exps)
      (apply-cont cont accum)
      (interp-exp/k (car exps)
                    env
                    (multi-exp-cont (cdr exps)
                                    accum-op
                                    accum
                                    env
                                    cont)))))

(define interp-exp/k
  (lambda (exp env cont)
    ; (display cont)(newline)(newline)
    (cases
      expression exp
      (const-exp
        (num)
        (apply-cont cont (numval num)))
      (diff-exp
        (minuend subtractor)
        (interp-exp/k minuend env (diff1-cont subtractor env cont)))
      (zero?-exp
        (exp)
        (interp-exp/k exp env (zero1-cont cont)))
      (if-exp
        (predicate sbj-exp else-exp)
        (interp-exp/k predicate env (if-test-cont sbj-exp else-exp env cont)))
      (var-exp
        (var)
        (apply-cont cont (deref (apply-env env var))))
      (let-exp
        (var exp1 body)
        (interp-exp/k exp1 env (let-exp-cont var env body cont)))
      (proc-exp
        (var body)
        (apply-cont cont (procval (closure var body env))))
      (call-exp
        (exp1 exp2)
        (interp-exp/k exp1 env (rator-cont exp2 env cont)))
      (letrec-exp
        (p-name b-var b-body letrec-body)
        (let ((new-env (extend-env-recursively p-name
                                               b-var
                                               b-body
                                               env)))
          (interp-exp/k letrec-body new-env cont)))
      (cons-exp
        (carv-exp cdrv-exp)
        (interp-exp/k carv-exp env (cons1-cont cdrv-exp env cont)))
      (car-exp
        (lst-exp)
        (interp-exp/k lst-exp env (car-exp-cont cont)))
      (cdr-exp
        (lst-exp)
        (interp-exp/k lst-exp env (cdr-exp-cont cont)))
      (empty-lst-exp
        ()
        (apply-cont cont (listval '())))
      (is-empty-exp
        (lst-exp)
        (interp-exp/k lst-exp env (is-empty-exp-cont cont)))
      (list-exp
        (exps)
        (interp-exps/k exps
                       (lambda (val accum)
                         (listval (append (expval->listval accum)
                                          (list val))))
                       (listval '())
                       env
                       cont))
      (compound-exp
        (exps)
        (interp-exps/k exps
                       (lambda (val accum)
                         val)
                       (listval '())
                       env
                       cont))
      (set-exp
        (var exp)
        (interp-exp/k exp
                      env
                      (set-rhs-cont (apply-env env var) env cont)))
      (mult-exp
        (exp1 exp2)
        (interp-exp/k exp1 env (mult-cont1 exp2 env cont)))
      )))

(define initial-env (empty-env))
(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (set! sigend 0)
        (initialize-store!)
        (let ((result (trampoline (interp-exp/k exp initial-env (end-cont)))))
          (if (= sigend 1)
            (expval->normalval result)
            (eopl:error 'interp "violate cps")
            ))))))

(define trampoline
  (lambda (bounce)
    (if (expval? bounce)
      bounce
      (trampoline (bounce)))))

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

   ; fact
   (test-prog-eqv "letrec fact(n) = if zero?(n)
                                    then 1
                                    else *(n, (fact -(n, 1)))
                   in (fact 4)"
                   24)
  )
