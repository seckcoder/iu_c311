#lang racket
(require racket/pretty
         "builtin.rkt"
         "../base/utils.rkt")

; InpExp -> (simple? SimpleExp)
(define (cps1 sexp)
  (match sexp
    [(or (? const? x)
         (? symbol? x)
         `(quote ,x)) (list #t sexp)]
    [(list (? op? op) rands ...)
     (if (andmap simple? rands)
       (list #t sexp)
       (list #f sexp))]
    [`(lambda (,vars ...) ,body)
      (let ((f-sym (gensym)))
        (list #t `(lambda ,(append vars (list f-sym))
                    ,(cps/k body (lambda (v)
                                   `(,f-sym ,v))))))]
    [_ (list #f sexp)]
    ))

(define (simple? sexp)
  (match (cps1 sexp)
    [(list simple? simple-exp)
     simple?]))

(define (inexp->simple sexp)
  (match (cps1 sexp)
    [(list simple? simple-exp)
     simple-exp]))

; InpExp * ((SimpleExp) -> tfExp) -> tfExp
(define (cps/k sexp builder)
  (match (cps1 sexp)
    [(list simple? simple-sexp)
     (if simple?
       (builder simple-sexp)
       (match sexp
         [(list (? op? op) rands ...)
          (cps-multi/k rands (lambda (simple-rands)
                               (builder `(,op ,@simple-rands))))]
         [`(if ,test ,then ,else)
           (cps/k test (lambda (simple-test)
                         `(if ,simple-test
                            ,(cps/k then builder)
                            ,(cps/k else builder))))]
         [(list rator rands ...)
          (cps-multi/k (cons rator rands)
                       (lambda (simple-exps)
                         (match simple-exps
                           [(list rator rands ...)
                            (let ((v-sym (gensym)))
                              `(,rator ,@rands (lambda (,v-sym)
                                                 ,(builder v-sym))))])))]
         ))]))

; InpExps * ((SimpleExps) -> TfExp) -> TfExp
(define (cps-multi/k sexps builder)
  (let-values ([(in-simple-exps in-tf-exps)
                (splitf-at sexps simple?)])
    (if (null? in-tf-exps)
      (builder (map inexp->simple sexps))
      (let ((in-a-tf-exp (car in-tf-exps))
            (in-rest-tf-exp (cdr in-tf-exps)))
        (cps/k in-a-tf-exp
               (lambda (v-sym)
                 (cps-multi/k `(,@in-simple-exps
                                 ,v-sym
                                 ,@in-rest-tf-exp)
                              builder)))))))

(define (cps sexp)
  (cps/k sexp (lambda (v)
                v)))

(module+ test
  (require rackunit)
  #|(check-eq? (cps 'a) 'a "symbol")
  (cps '(f a))
  (cps '(f (g a)))
  (cps '((f a) (g b)))
  (cps '(if a
          (f a)
          (f b)))
  (cps '(if (f a)
          (f b)
          (f c)))
  (cps '(+ 1 2 a))
  (cps '(+ (f a) 3))
  (pretty-print (cps '(+ 3 (+ 4 (f a)))))
  (pretty-print
    (cps '(lambda (n)
            ((lambda (mk)
               ((mk mk) n))
             (lambda (mk)
               (lambda (n)
                 (if (= n 0)
                   1
                   (* n ((mk mk) (- n 1))))
                 ))))))
  (cps '(lambda (x) (if (f x) a b)))|#
  ; (pretty-print (cps '(((f a) (g b)) ((f c) (g d)))))
  (pretty-print (cps '(lambda (x)
                        (if x
                          a
                          b))))
  (pretty-print (cps '(lambda (x)
                        (if (if x
                              (f a)
                              b)
                          c
                          d))))
  (pretty-print (cps '(lambda (x)
                        (if (if t
                              (if x
                                (f a)
                                b)
                              c)
                          e
                          w))))
  (pretty-print (cps '(lambda (x)
                        (if (f a)
                          (if b
                            (f b)
                            (f c))
                          (if c
                            (f a)
                            (f b))))))
  )
