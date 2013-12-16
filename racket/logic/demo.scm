(load "mk.scm")
(load "utils.scm")

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
