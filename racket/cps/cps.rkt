#lang eopl

(require "../base/utils.rkt")
(require racket/match)

(define-datatype
  continuation continuation?
  (end-cont)
  (call-rand-cont
    (cps-simple-rator sexp?)
    (cont continuation?))
  (call-rator-cont
    (rand sexp?)
    (cont continuation?))
  )

(define cps
  (lambda (sexp cont)
    (match sexp
      [(? const? x) (apply-cont cont sexp #t)]
      [(? symbol? x) (apply-cont cont sexp #t)]
      [`(quote ,x) (apply-cont cont sexp #t)]
      #|[(list (? op? op) params* ...)
       (op-exp op (map parse params*))]
      [`(lambda (,params ...) ,body)
        (lambda-exp params
                    (parse body))]
      [`(if ,test ,then ,else)
        (if-exp (parse test)
                (parse then)
                (parse else))]
      [`(if ,test ,then)
        (cps `(if ,test ,then (void)))]|#
      [(list rator rand)
       (cps rator (call-rator-cont rand cont))]
      )))

(define is-end-cont
  (lambda (k)
    (cases continuation k
      (end-cont
        ()
        #t)
      (else
        #f))))

(define apply-cont
  (lambda (cont sexp simple?)
    (cases continuation cont
      (end-cont
        ()
        sexp)
      (call-rand-cont
        (cps-simple-rator saved-cont)
        (let ((cps-rand sexp))
          (if simple?
            (apply-cont saved-cont
                        (list cps-simple-rator cps-rand) #f)
            (let* ((v (gensym))
                   (sub-sexp (apply-cont saved-cont
                                      (list cps-simple-rator v)
                                      #f)))
              (append cps-rand (list `(lambda (,v)
                                        ,sub-sexp)))))))
      (call-rator-cont
        (rand saved-cont)
        (let ((cps-rator sexp))
          (if simple?
            (cps rand (call-rand-cont cps-rator saved-cont))
            (let* ((g (gensym))
                   (sub-sexp (cps (list g rand) saved-cont)))
              (append cps-rator (list `(lambda (,g)
                                         ,sub-sexp)))
              ))))
    )))


(define (test)
  ; (cps '((f a) b) (end-cont))
  ; (cps '(((f a) b) c) (end-cont))
  ; (cps '(f a) (end-cont))
  ; (cps '(f (g a)) (end-cont))
  (cps '((f a) (g b)) (end-cont))
  )
