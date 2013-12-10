#lang eopl
(require "../base/utils.rkt")
(require racket/match)
(define op?
  (lambda (op)
    (memq op '(cons list car cons
                    - = * / +))))
(define simple?
  (lambda (sexp)
    (match sexp
      [(or (? const? x)
           (? symbol? x)
           `(quote ,x)) #t]
      [(list (? op? op) params* ...)
       (andmap simple? params*)]
      [`(if ,test ,then ,else)
        (andmap simple? (list test then else))]
      [`(lambda (,x ,k) ,body) #t]
      [`(lambda (,x) ,body) #f]
      [(list rator rands* ...) #f])))

(define append-if-not-null
  (lambda (sexp k)
    (if (null? k)
      sexp
      (append sexp (list k)))))
(define cps-rands/k
  (lambda (rator rands k)
    (let-values ([(simple-rands tf-rands)
                  (splitf-at rands simple?)])
      (if (null? tf-rands)
        (cps/k (cons rator simple-rands) k)
        (let ((rand (car tf-rands))
              (rest-tf-rands (cdr tf-rands)))
          (cps/k rand (lambda (v-sym)
                        (cps-rands/k rator
                                     `(,@simple-rands
                                        ,v-sym
                                        ,@rest-tf-rands)
                                     k))))))))
(define cps/k
  (lambda (sexp k)
    (match sexp
      [(? simple? sexp) (k sexp)]
      [`(if ,test ,then ,else)
        ; returned value is in then and else
        (if (simple? test)
          `(if ,test
             ,(cps/k then k)
             ,(cps/k else k))
          (cps/k test (lambda (v)
                        (cps/k `(if ,v
                                  ,then
                                  ,else)
                               k))))]
      [(list (? op? op) params* ...)
       (if (andmap simple? params*)
         (k sexp)
         (cps-rands/k op params* k))]
      [`(lambda (,x) ,body)
        ; lambda is the returned value
        (k
          (let ((k-sym (gensym)))
            (if (simple? body)
              `(lambda (,x ,k-sym)
                 (,k-sym ,x))
              `(lambda (,x ,k-sym)
                 ,(cps/k body (lambda (v)
                                `(,k-sym ,v)))))))]
      [(list rator rand)
       ; k:
       ; what should we do with the returned value v?
       ; use it to fill the hole of rator
       (let ((res-sym (gensym)))
         (cond ((andmap simple? (list rator rand))
                `(,rator ,rand (lambda (,res-sym)
                                    ,(k res-sym))))
               ((simple? rator)
                (cps-rands/k rator (list rand) k))
               (else
                 (cps/k rator (lambda (f)
                                (cps/k `(,f ,rand) k))))))]
      )))

(define (cps sexp)
  (cps/k sexp (lambda (v)
                v)))

(define (test)
  (cps '((f a) b))
  ; (cps '(((f a) b) c) (end-cont))
  ; (cps '(f a) (end-cont))
  (cps '(f (g a)))
  (cps '((f a) (g b)))
  (cps '((g ((f a) b)) (h c)))
  (cps '(f (if (g a)
             (g b)
             (g c))))
  (cps '(+ (g a) 1))
  (cps '(g (+ (f b) 1)))
  (cps '(lambda (n)
          ((lambda (mk)
             ((mk mk) n))
           (lambda (mk)
             (lambda (n)
               (if (= n 0)
                 1
                 (* n ((mk mk) (- n 1))))
               )))))
  (cps '((lambda (v) v) 1))
  (cps '((lambda (v) (f (g v))) 1))

  'ok
  )

(define (utest)
  (let* ((fact-prog '(lambda (n)
                       ((lambda (mk)
                          ((mk mk) n))
                        (lambda (mk)
                          (lambda (n)
                            (if (= n 0)
                              1
                              (* n ((mk mk) (- n 1))))
                            )))))
         (fact-cps-prog (cps fact-prog))
         (fact/k (eval fact-cps-prog))
         (fact (eval fact-prog))
         (fact-cps (lambda (n)
                     (fact/k n (lambda (v) v)))))
    (println "***********************cps transformation**********************")
    (pretty-print fact-prog)
    (println "*************************to**************************************")
    (pretty-print fact-cps-prog)
    (newline)
    (println (fact 10))
    (println (fact-cps 10))))
