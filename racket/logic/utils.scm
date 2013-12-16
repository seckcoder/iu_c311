; This implementation of caro is wrong. Why?
#|(define caro|#
  ;(lambda (lst q)
    #|(== (car lst) q)))|#

(define caro
  (lambda (lst a)
    (fresh (d)
      ;(== (cons a d) lst))))
      (conso a d lst))))

(define cdro
  (lambda (lst d)
    (fresh (a)
      ;(== (cons a d) lst))))
      (conso a d lst))))

(define conso
  (lambda (a d lst)
    (== (cons a d) lst)))

(define nullo
  (lambda (lst)
    (== lst '())))

(define eqo
  (lambda (x y)
    (== x y)))

(define pairo
  (lambda (p)
    (fresh (a d)
      ;(== (cons a d) p))))
      (conso a d p))))


(define listo
  (lambda (l)
    (conde
      ((nullo l))
      ((pairo l)
       (fresh (d)
         (cdro l d)
         (listo d))))))

; list of list
(define lolo
  (lambda (l)
    (conde
      ((nullo l))
      ((fresh (a)
         (caro l a)
         (listo a))
       (fresh (d)
         (cdro l d)
         (lolo d)))
      )))

(define twinso
  (lambda (s)
    (fresh (x y)
      (conso x y s)
      (conso x '() y))))

; list of twins
(define loto
  (lambda (l)
    (conde
      ((nullo l))
      ((fresh (a)
         (caro l a)
         (twinso a))
       (fresh (d)
         (cdro l d)
         (loto d)))
      )))
