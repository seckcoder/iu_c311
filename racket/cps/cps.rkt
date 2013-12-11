#lang racket

(require racket/pretty)
(require eopl/datatype)
(require "../base/utils.rkt")
(require (submod "parser.rkt" ds))

(define m-gensym
  (let ((n 0))
    (lambda (prefix)
      (let ((v-sym (string->symbol (string-append prefix (number->string n)))))
        (set! n (+ n 1))
        v-sym))))

(define var-sym
  (let ((n 0))
    (lambda ()
      (m-gensym "v"))))

(define fun-sym
  (let ((n 0))
    (lambda ()
      (m-gensym "f"))))


; InpExps * ((SimpleExps) -> TfExp) -> TfExp
(define cps-exps
  (lambda (exps builder)
    (let-values ([(in-simple-exps in-tf-exps)
                  (splitf-at exps simple?)])
      (if (null? in-tf-exps)
        (builder (map cps-simple-exp in-simple-exps))
        (let ((a-in-tf-exp (car in-tf-exps))
              (rest-in-tf-exps (cdr in-tf-exps))
              (v-sym (var-sym)))
          (cps/k a-in-tf-exp
                 (cps-lambda-exp
                   (list v-sym)
                   (cps-exps `(,@in-simple-exps
                                ,(var-exp v-sym)
                                ,@rest-in-tf-exps)
                             builder))))))))

; InpExp * SimpleExp -> TfExp
(define cps/k
  (lambda (exp k-exp)
    (match (cps-simple exp)
      [(list simple? cpsed-exp)
       (if simple?
         (cps-call-exp k-exp (list cpsed-exp))
         (cases expression exp
           (call-exp
             (rator rands)
             (cps-exps (cons rator rands)
                       (lambda (simple-exps)
                         (match simple-exps
                           [(list simple-rator simple-rands ...)
                            (cps-call-exp simple-rator
                                          (append simple-rands (list k-exp)))]
                           ))))
           (op-exp
             (op rands)
             (cps-exps rands
                       (lambda (simple-rands)
                         (cps-call-exp k-exp (list (cps-op-exp op simple-rands))))))
           (if-exp
             (test then else)
             (cps-exp test (lambda (simple-test)
                             (cps-if-exp simple-test
                                         (cps/k then k-exp)
                                         (cps/k else k-exp)))))
           (letrec-exp
             (p-names procs body)
             (cps-exps procs (lambda (cps-procs)
                               (cps-letrec-exp p-names
                                               cps-procs
                                               (cps/k body k-exp)))))
           (else
             (begin
               (printf "~v is not supported\n" exp)
               (error "not supported"))
             ))
         )])))
(define cps-exp
  (lambda (exp builder)
    (cps-exps (list exp)
              (lambda (simple-exps)
                (builder (car simple-exps))))))
(define cps
  (lambda (exp)
    (cps-exp exp (lambda (simple-exp)
                   (simple-exp->exp simple-exp)))))

(define cps-simple
  (lambda (exp)
    (cases expression exp
      (const-exp
        (cst) (list #t (cps-const-exp cst)))
      (var-exp
        (var) (list #t (cps-var-exp var)))
      (quote-exp
        (qexp) (list #t (cps-quote-exp qexp)))
      (op-exp
        (op rands) (if (andmap simple? rands)
                     (list #t (cps-op-exp op (map cps-simple-exp rands)))
                     (list #f exp)))
      (lambda-exp
        (vars body)
        (let ((f-sym (fun-sym)))
          (list #t (cps-lambda-exp (append vars (list f-sym))
                                   (cps/k body (cps-var-exp f-sym))))))
      (else
        (list #f exp))
      )))

(define cps-simple-exp
  (lambda (exp)
    (match (cps-simple exp)
      [(list simple? simple-exp)
       (if simple?
         simple-exp
         (error "not a simple expression"))])))

(define simple?
  (lambda (exp)
    (car (cps-simple exp))))

(module+ test
  (require rackunit)
  (define test-cps
    (lambda args 
      (match args
        [(list) (void)]
        [(list in-prog out-prog desc rest ...)
         (check-equal? (out:unparse (cps (in:parse in-prog)))
                       out-prog
                       desc)
         (apply test-cps rest)])))
  (test-cps 'a 'a "simple var")
  (test-cps ''a ''a "simple symbol")
  (test-cps '(f a) '(f a (lambda (v0)
                           v0)) "proc call")
  (test-cps '(f (g a)) '(g a (lambda (v2)
                               (f v2 (lambda (v1)
                                       v1)))) "proc call")
  (test-cps '((f a) (g b)) '(f a (lambda (v4) (g b (lambda (v5) (v4 v5 (lambda (v3) v3)))))) "proc call")
  (test-cps '(+ a b) '(+ a b) "op call")
  (test-cps '(+ (f a) b) '(f a (lambda (v7) ((lambda (v6) v6) (+ v7 b)))) "op + proc")
  (test-cps '(if a
               (f b)
               (f c)) '(if a (f b (lambda (v8) v8)) (f c (lambda (v8) v8))) "if")
  (test-cps '(if (f a)
               (f b)
               (f c)) '(f a (lambda (v10) (if v10 (f b (lambda (v9) v9)) (f c (lambda (v9) v9))))) "if")
  (test-cps '((lambda (a)
                (f a))
              3) '((lambda (a f13) (f a f13)) 3 (lambda (v11) v11)) "lambda")
  #|(out:unparse (cps (in:parse ')))|#
  )

(module+ test
  (require "tail-form.rkt"
           "parser.rkt")
  (define test-result
    (lambda args
      (match args
        [(list) (void)]
        [(list in-prog result desc rest ...)
         (check-equal? (interp (cps (in:parse in-prog)))
                       result
                       desc)
         (apply test-result rest)]
        )))
  (test-result '3 3 "number")
  (test-result '((lambda (v)
                   v)
                 4) 4 "lambda")
  (test-result '((lambda (n)
                  ((lambda (mk)
                     ((mk mk) n))
                   (lambda (mk)
                     (lambda (n)
                       (if (= n 0)
                         1
                         (* n ((mk mk) (- n 1))))
                       )))) 10) 3628800 "u-combinator")
  (test-result '(let ((foo (lambda (v)
                             v)))
                  (foo 4)) 4 "let exp")
  (test-result '(letrec ((fact (lambda (n)
                                 (if (= n 0)
                                   1
                                   (* n (fact (- n 1)))))))
                  (fact 10)) 3628800 "letrec")
  )
