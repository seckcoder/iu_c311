#lang racket

(require "../../base/utils.rkt")

; A simple constraint system fit for follow-set

(provide Var
         Var?
         st-var
         st-var?
         unify*
         unify
         equa
         equa-=
         equa-<=
         empty-subst
         subst
         subst->list
         print-subst
         )

(struct st-var (v) #:transparent)
(define (Var [v 'v])
  (st-var v))
(define Var? st-var?)
(struct equa (v w) #:transparent)
(struct equa-= equa () #:transparent)
(struct equa-<= equa () #:transparent)
(struct subst (=s <=s) #:transparent)

(define (empty-subst)
  (subst '() '()))

(define (subst->list s) s)

(define (unify s e)
  (match e
    [(equa-= v w)
     (match s
       [(subst =s <=s)
        (subst (unify-= v w =s)
               <=s)])]
    [(equa-<= v w)
     (unify-<= v w s)]
    [_ (error 'unify "match failed")]
    ))

(define (unify* s . es)
  (foldl
    (lambda (e s)
      (unify s e))
    s
    es))

(define (unify-= v w =s)
  (let ((v (walk =s v))
        (w (walk =s w)))
    (if (equal? v w)
      =s
      (ext-=s =s v w))))

; lst [a,b,c,v,d,...]
; -> [a,b,c,v]
(define (member-<= v lst)
  (let loop ((lst lst)
             (acc '()))
    (cond ((null? lst) '())
          ((equal? (car lst) v)
           (reverse (cons v acc)))
          (else
            (loop (cdr lst)
                  (cons (car lst) acc))))))

(define (equa-assoc v s)
  (findf (lambda (eq)
           (equal? (equa-v eq) v))
         s))

(define (walk** s v)
  (match (equa-assoc v s)
    [#f '()]
    [(equa v w1)
     (if (Var? w1)
       (cons w1
             (walk** s w1))
       (list w1))]
    [_ (error 'walk** "match failed")]
    ))

(define (remove-s s eq)
  #|(print-subst '= s)
  (print-equa '= eq)|#
  (if (not (member eq s))
    (error 'remove-s "equation:~a is not in\n\tsubstituion:~a" eq s)
    (filter (lambda (eq1)
              (not (equal? eq eq1)))
            s)))

(define (unify-<= v w s)
  (match s
    [(subst =s <=s)
     ; there is bug here. a <= b, a <= c; (walk** <=s 'a) will only
     ; get {b} or {c}
     (let ((v<=* (walk** <=s v))
           (w<=* (walk** <=s w)))
       (cond ((and (member w v<=*)
                   (member v w<=*))
              (error 'unify-<= "bad s"))
             ((member v w<=*)
              ; w <= v => v = w
              (let loop ((v w)
                         (ws (member-<= v w<=*))
                         (=s =s)
                         (<=s <=s))
                (cond
                  ((null? ws)
                   (subst =s <=s))
                  (else
                    (let ((w (car ws)))
                      (loop w
                            (cdr ws)
                            (unify-= v w =s)
                            (remove-s <=s (equa-<= v w))))))))
             ((member w v<=*)
              ; v <= w is in <=s
              (subst =s <=s))
             (else
               (let ((v (walk =s v))
                     (w (walk =s w)))
                 (if (equal? v w)
                   (subst =s <=s)
                   (subst =s
                          (ext-<=s <=s v w)))))))]
    [_ (error 'unify-<= "match failed")]
    ))

(define (walk s v)
  (match (equa-assoc v s)
    [#f v]
    [(equa v w) w]
    [_ (error 'walk "match failed")]
    ))

(define (ext-=s =s v w)
  (cons (equa-= v w)
        (map (match-lambda
               [(equa-= v1 w1)
                (equa-= v1 (replace w1 v w))]
               [_ (error 'ext-=s "match failed")]
               )
             =s)))
(define (ext-<=s <=s v w)
  (cons (equa-<= v w)
        <=s))

(define (replace w1 v w)
  (if (equal? w1 v)
    w
    w1))


(define print-equa
  (match-lambda*
    [(list sym
     (or (equa (st-var v) (st-var w))
         (equa (st-var v) w)
         (equa v (st-var w))
         (equa v w)))
     (printf "~a ~a ~a\n" v sym w)]))

(define print-subst
  (match-lambda*
    [(list (subst =s <=s))
     (print-subst '= =s)
     (print-subst '<= <=s)]
    [(list (? symbol? sym) (? (list-of equa?) s))
     (for-each
       (lambda (eq)
         (print-equa sym eq))
       s)]))

(module+ test
  (require rackunit)
  (let ((s (empty-subst)))
    (print-subst
      (unify* s
              (equa-<= (Var 'a)
                       (Var 'b)))))

  (printf "\n\n")
  (let ((s (empty-subst)))
    (let ((s
            (unify* s
                    (equa-<= (Var 'a)
                             (Var 'b))
                    (equa-<= (Var 'b)
                             (Var 'd))
                    (equa-<= (Var 'c)
                             (Var 'd))
                    (equa-= (Var 'a)
                            (Var 'e)))))
      (print-subst s)
      (printf "\n")
      (print-subst
        (unify s (equa-<= (Var 'd)
                          (Var 'a))))))

  (printf "\n\n")

  (let ((s (empty-subst)))
    (let ((s
            (unify* s
                    (equa-<= (Var 'a)
                             (Var 'b))
                    (equa-<= (Var 'b)
                             (Var 'd))
                    (equa-<= (Var 'c)
                             (Var 'd))
                    (equa-= (Var 'a)
                            (Var 'e)))))
      (print-subst s)
      (printf "\n")
      (print-subst
        (unify s (equa-<= (Var 'a)
                         (Var 'd))))))

  (printf "\n\n")
  (let ((s (empty-subst)))
    (let ((s
            (unify* s
                    (equa-<= (Var 'a)
                             (Var 'b))
                    (equa-<= (Var 'b)
                             (Var 'd))
                    (equa-<= (Var 'c)
                             (Var 'd))
                    (equa-= (Var 'a)
                            (Var 'e)))))
      (print-subst s)
      (printf "\n")
      (print-subst
        (unify s (equa-<= (Var 'a)
                          (Var 'e))))))

  (printf "\n\n")

  (let ((s (empty-subst)))
    (let ((s
            (unify* s
                    (equa-<= (Var 'a)
                             (Var 'b))
                    (equa-<= (Var 'b)
                             (Var 'd))
                    (equa-<= (Var 'c)
                             (Var 'd))
                    (equa-= (Var 'a)
                            (Var 'e)))))
      (print-subst s)
      (printf "\n")
      (print-subst
        (unify s (equa-<= (Var 'a)
                          (Var 'c))))))
)
