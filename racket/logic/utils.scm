; This implementation of caro is wrong. Why?
#|(define caro|#
    ;(lambda (lst q)
    #|(== (car lst) q)))|#

(define teacupo
  (lambda (x)
    (conde
      ((== 'tea x))
      ((== 'cup x)))))

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
#|(define loto
    (lambda (l)
      (conde
        ((nullo l))
        ((fresh (a)
           (caro l a)
           (twinso a))
         (fresh (d)
           (cdro l d)
           (loto d)))
        )))|#

(define loto
  (lambda (l)
    (listofo twinso l)))

(define listofo
  (lambda (predo l)
    (conde
      ((nullo l))
      ((fresh (a)
         (caro l a)
         (predo a))
       (fresh (d)
         (cdro l d)
         (listofo predo d))))))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membero
  (lambda (x l)
    (conde
      ; whenever a conde line is guaranteed to
      ; fail, it's unnecessary. But it's better
      ; to leave it for documenting purpose
      ; ((nullo l) fail)
      ((eq-caro l x))
      ((fresh (d)
         ; the order influences the result.
         ; swap the order of cdro and membero
         ; the program will hang there for some cases of run*
         ; . Why?...
         (cdro l d)
         (membero x d)
         )))))

(define pmembero
  (lambda (x l)
    (conde
      ; whenever a conde line is guaranteed to
      ; fail, it's unnecessary. But it's better
      ; to leave it for documenting purpose
      ; ((nullo l) fail)
      ((eq-caro l x)
       (cdro l '()))
      ((eq-caro l x)
       (fresh (a d)
         (cdro l (cons a d))))
      ((fresh (d)
         (cdro l d)
         (pmembero x d))))))

; apparently, this doens't work...
(define memberrevo
  (lambda (x l)
    (conde
      ((fresh (d)
         (cdro l d)
         (memberrevo x d)))
      ((eq-caro l x)))))

(define memo
  (lambda (x l out)
    (conde
      ((eq-caro l x)
       (== l out))
      ((fresh (d)
         (cdro l d)
         (memo x d out)))
      )))

; remove a member x or not remove
(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== out '()))
      ((eq-caro l x)
       (cdro l out))
      ((fresh (d d-r a)
         ; Note here we didn't request a != x
         (conso a d l)
         (rembero x d d-r)
         (conso a d-r out))))))

(define surpriso
  (lambda (r)
    ; The problem with the supriso
    ; is that rembero could pass even if we don't remove x
    ; when x equals b. To solve
    ; the problem, we need introduce negation operator
    ; and request a != x in the last condition.
    (rembero r '(a b c) '(a b c))))

; append a list l with s into out
; Note l should be a list according to the definition,
; but s should not have to be.
(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== out s))
      ((fresh (a d res)
         (conso a d l)
         ;!!!
         (conso a res out)
         (appendo d s res)
         )))))

(define swappendo
  (lambda (l s out)
    (conde
      ((fresh (a d res)
         (conso a d l)
         (conso a res out)
         (swappendo d s res)))
      ((nullo l) (== out s)))))

(define unwrapo
  (lambda (x out)
    (conde
      ((== x out))
      ((pairo x)
       (fresh (a)
         (caro x a)
         (unwrapo a out)))
      )))

(define flatteno
  (lambda (s out)
    (conde
      ((nullo s)
       (== out s))
      ((conso s '() out))
      ((fresh (a d res1 res2)
         (conso a d s)
         (flatteno a res1)
         (flatteno d res2)
         (appendo res1 res2 out)))
      )))

(define anyo
  (lambda (g)
    (conde
      (g)
      (else (anyo g)))))

(define nevero (anyo fail))

(define alwayso (anyo succeed))

(define salo
  (lambda (g)
    (conde
      (succeed)
      (g))))

(define identify
  (lambda (l)
    (fresh (q)
      (membero q l))))

(define first-value
  (lambda (l)
    (run 1 (y)
      (membero y l))))

(define reverse-list
  (lambda (l)
    (run* (y)
      (memberrevo y l))))
