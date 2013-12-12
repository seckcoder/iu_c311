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
                    ,(cps/k-exp body f-sym))))]
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

(define (replace body var sexp)
  (match body
    [(or (? const? x)
         `(quote ,x)) body]
    [(or (? symbol? x))
     (if (eq? x var)
       sexp
       x)]
    [`(lambda (,vars ...) ,sub-body)
      (if (mfindf (lambda (t)
                   (eq? var t))
                 vars)
        body
        `(lambda ,vars
           ,(replace sub-body var sexp)))]
    [`(if ,test ,then ,else)
      `(if ,(replace test var sexp)
         ,(replace then var sexp)
         ,(replace else var sexp))]
    [(list (? op? op) rands ...)
     (cons op (map (lambda (rand)
                     (replace rand var sexp)) rands))]
    [(list rator rands ...)
     (map (lambda (subsexp)
            (replace subsexp var sexp))
          (cons rator rands))]
    ))

(define (send-cont k-exp simple-sexp)
  (match k-exp
    [`(lambda (,vars ...) ,body)
      (replace body (car vars) simple-sexp)]
      #|`(let ((,@vars ,simple-sexp))
         ,body)]|#
    [_ `(,k-exp ,simple-sexp)]))

; InpExp * SimpleExp -> tfExp
(define (cps/k-exp sexp k-exp)
  (match (cps1 sexp)
    [(list is-simple? simple-sexp)
     (if is-simple? 
       (send-cont k-exp simple-sexp)
       (match sexp
         [(list (? op? op) rands ...)
          (cps-multi/k rands (lambda (simple-rands)
                               (send-cont k-exp `(,op ,@simple-rands))))]
         [`(if ,test ,then ,else)
           (cps/k test (lambda (simple-test)
                         (cond ((symbol? k-exp)
                                `(if ,simple-test
                                   ,(cps/k-exp then k-exp)
                                   ,(cps/k-exp else k-exp)))
                               (else
                                 `(let ((k ,k-exp))
                                    (if ,simple-test
                                      ,(cps/k-exp then `k)
                                      ,(cps/k-exp else `k)))))))]
         [(list rator rands ...)
          (cps-multi/k (cons rator rands)
                       (lambda (simple-exps)
                         (match simple-exps
                           [(list rator rands ...)
                            `(,rator ,@rands ,k-exp)])))]
         ))]))

; 6.29
; InpExps * ((SimpleExps) -> TfExp) -> TfExp
(define (cps-multi/k sexps builder)
  (let cps-of-rest ((sexps sexps)
                    (acc '()))
    (cond ((null? sexps)
           (builder (map inexp->simple (reverse acc))))
          ((simple? (car sexps))
           (cps-of-rest (cdr sexps)
                        (cons (car sexps)
                              acc)))
          (else
            (cps/k (car sexps)
                   (lambda (v)
                     (cps-of-rest (cdr sexps)
                                  (cons v acc))))))))

; 6.30
; InpExp * ((SimpleExp) -> TfExp) -> TfExp
(define (cps/k sexp builder)
  (match (cps1 sexp)
    [(list is-simple? simple-sexp)
     (if is-simple?
       (builder simple-sexp)
       (let ((v-sym (gensym)))
         (cps/k-exp sexp `(lambda (,v-sym)
                            ,(builder v-sym)))))]))

(define (cps sexp)
  (cps/k sexp (lambda (v)
                v)))

(module+ test
  (require rackunit)
  (check-eq? (cps 'a) 'a "symbol")
  #|(cps '(f a))
  (cps '(f (g a)))
  (cps '((f a) (g b)))
  (cps '(if a
          (f a)
          (f b)))
  (cps '(if (f a)
          (f b)
          (f c)))
  (pretty-print
  (cps '(if (if (f a)
              (f b)
              (f c))
          (f b)
          (f c)))
  )|#
  (cps '(if a
    (f a)
    b))
  #|(pretty-print
  (cps '(if (if a
                 (f a)
                 b)
             (f a)
             b))
  )|#
  #|(cps '(+ 1 2 a))
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

  ; test for 6.22
  ; (cps '(+ (f a) 3))


  ; test for 6.23
  #|(cps '(if a
          (f a)
          (f b)))
  (pretty-print
  (cps '(if (if a
              (f a)
              b)
          (f a)
          b)))|#

  ; test for 6.26 
  #|(cps '(+ (f a) 4))
  (pretty-print (cps '(+ 3 (+ 4 (f a)))))|#
  )
