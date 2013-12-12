#lang racket

(require eopl/datatype
         "store.rkt"
         "parser.rkt"
         "../base/utils.rkt"
         "env.rkt")

(define (typeof-const v)
  (cond ((number? v) 'int)
        ((string? v) 'str)
        ((boolean? v) 'bool)))

(define (typeof-sexp v)
  (cond ((atom? v) 'atom)
        ((list? v) 'list)))

; get printable version of type names
(define (pr t)
  (match t
    [(? symbol? t) t]
    [(list (list var-types ...) ret-type)
     (format "~s -> ~s" var-types ret-type)]
    [_ (error 'pr "pr match error:~s" t)]))

(define (check-equal-type! t1 t2 exp)
  (if (not (equal? t1 t2))
    (error 'Type-Match "~s not match ~s in ~a" (pr t1) (pr t2) exp)
    (void)))

(define (vartype-of-lambda exp)
  (cases expression exp
    (lambda-exp
      (vars types body)
      types)
    (else
      (error 'vartype-of-lambda "~s is not a lambda expression" exp))))


(define (typeof exp tenv)
  (cases expression exp
    (const-exp
      (cst)
      (typeof-const cst))
    (var-exp
      (var)
      (deref (apply-env tenv var)))
    (quote-exp
      (sexp)
      (typeof-sexp sexp))
    (op-exp
      (op rands)
      (cond ((memq op '(+ - * / =))
             (for-each (lambda (rand)
                         (check-equal-type! (typeof rand tenv)
                                            'int
                                            rand))
                       rands)
             'int)
            ((memq op '(zero?))
             (check-equal-type! (typeof (car rands) tenv)
                                'int
                                (car rands))
             'bool)
            (else
              (error 'op "not supported"))))
    (if-exp
      (test then else)
      (check-equal-type! (typeof test tenv)
                         'bool
                         test)
      (let ((then-type (typeof then tenv))
            (else-type (typeof else tenv)))
        (check-equal-type! then-type else-type exp)
        then-type))
    (lambda-exp
      (vars types body)
      (let ((new-tenv (extend-envs vars (newrefs types) tenv)))
        (let ((ret-type (typeof body new-tenv)))
          (list types ret-type))))
    (call-exp
      (rator rands)
      (match (typeof rator tenv)
        [(list (list rand-types ...) result-type)
         (for-each (lambda (rand-type1 rand-type2)
                     (check-equal-type! rand-type1
                                        rand-type2
                                        exp))
                   rand-types (map (lambda (rand)
                                     (typeof rand tenv))
                                   rands))
         result-type]
        [_ (error "call exp match error")]))
    (compound-exp
      (exps)
      (typeof (tail exps) tenv))
    (letrec-exp
      (p-names ret-types procs body)
      (let* ((p-types 
               (map (lambda (ret-type proc)
                      (let ((var-types (vartype-of-lambda proc)))
                        (list var-types ret-type)))
                    ret-types procs))
             (new-tenv (extend-envs p-names (newrefs p-types) tenv))
             (_ (map (lambda (proc p-type)
                       ; just a verification
                       (check-equal-type! (typeof proc new-tenv)
                                          p-type
                                          proc))
                     procs p-types)))
        (typeof body new-tenv)))
    ))

(module+ test
  (define (test-typeof prog)
    (initialize-store!)
    (typeof (parse prog) (empty-env))
    )
  )

(module+ test
  #|(pr (test-typeof '1))
  (pr (test-typeof '"a"))
  (pr (test-typeof ''a))
  (pr (test-typeof ''(1 a 3)))
  (pr (test-typeof '(+ 1 2)))
  (pr (test-typeof '(lambda ((a int)
                             (b str))
                      (+ a 2))))
  (pr (test-typeof '(letrec (((foo int) (lambda ((v int))
                                          (foo 2))))
                      (foo 3))))|#
  )

(module+ test
  #|(pr (test-typeof '(letrec (((foo int) (lambda ((v int))
                                          (foo 'a))))
                      (foo 3))))|#
  (pr (test-typeof '(letrec (((foo int) (lambda ((v int))
                                          "a")))
                      (foo 3))))
  )
