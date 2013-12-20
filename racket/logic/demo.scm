(load "mk.scm")
(load "utils.scm")

(run* (q)
  (== q #t))

(run* (q)
  (listo `(a b ,q c)))

(run 2 (q)
  (listo `(a b c . ,q)))

(run 5 (q)
  (lolo `((a b) (c) ,q)))

(run 5 (q)
  (lolo `((a b) (c) . ,q)))

(run* (q)
  (twinso '(tofu tofu))
  (== #t q))

(run 2 (q)
  (membero 'e q))

(run 1 (q)
  (fresh (x l)
    (membero x l)
    (== q #t)))

(run 2 (l)
  (membero 'e l))

(run* (q)
  (fresh (x)
    (pmembero x '(e e))
    (== q #t)))


(run* (q)
  (memberrevo q '(1 2)))

(run* (q)
  (memo 'a '(c d a g b c a e) `(a . ,q)))

(run 10 (q)
  (fresh (u)
    (memo 'tofu q u)))


(run 1 (out)
  (fresh (y)
    (rembero 'peas `(a b ,y d peas e) out)))

(run* (out)
  (fresh (y z)
    (rembero y `(a b ,y d ,z e) out)))

(run* (out)
  (rembero 'a '(a a) out))

(run* (w)
  (fresh (y z out)
    (rembero y `(a b ,y d ,z . w) out)))

(run* (v)
  (== v 'b)
  (surpriso v))

(run 5 (q)
  (fresh (y)
    (appendo `(a . ,y) '(d e) q)))

(run 5 (x)
  (fresh (y)
    (appendo x y '(cake with ice d t))))

(run 2 (x)
  (fresh (y)
    (appendo x y '(a))))

(run* (x)
  (fresh (y z)
    (conso x y z)))


(run 7 (z)
  (fresh (y x)
    (appendo x y z)))

(run 1 (z)
  (fresh (x y)
    (swappendo x y z)))

(run 12 (x)
  (unwrapo '(((pizza))) x))
(run 2 (x)
  (unwrapo `(,x) 'pizza))

(run 1 (x)
  (unwrapo `((,x)) 'pizza))

(run 2 (l)
  (flatteno '((a b) c) l))

(run 3 (l)
  (flatteno '(a) l))

(run 2 (l)
  (flatteno l '(a)))

(run 5 (q)
  alwayso
  (== q #t))

(run 2 (q)
  (salo alwayso)
  (== q #t))

(run 1 (q)
  alwayso
  fail
  (== q #t)
  )

(run 1 (q)
  (condi
    ((== #f q) alwayso)
    (else (== #t q)))
  (== #t q))

(run 2 (q)
  (== #t q))

(run 2 (q)
  (condi
    (fail)
    ((== #t q)))
  (== #t q))

(run 2 (q)
  (condi
    ((== #f q) alwayso)
    (else ))
  (== #t q))


(run* (r)
  (conde
    (fail)
    ((== 'cup r))))

(run 5 (r)
  (condi
    ((teacupo r))
    ((== #f r))))

(define foo
  (lambda ()
    (conde
      (fail)
      (succeed))))

(define foo-o (foo))

(run 1 (q)
  nevero)

(run 2 (q)
  (condi
    (alwayso)
    (foo-o)
    )
  (== #t q))

(run 1 (q)
  (alli
    (conde
      ((== #f q))
      ((== #t q)))
    alwayso
    )
  (== #t q))

(run* (q)
  (>1o '(0 1 1))
  (== #t q))

(run* (q)
  (poso '(0 1 1))
  (== #t q))

(run* (x)
  (fresh (y z)
    (== x y)
    (== y z)
    (== z x)))

(run* (x)
  (all
    (conde
      ((== x #f))
      ((== x #t)))
    (== x 'c)))

(run* (q)
  (conde
    ((== q #f))
    ((== q #t)))
  (== q #t))
