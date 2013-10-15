(library
  (compat)
  (export add1 sub1 atom?)
  (import (rnrs))

  (define add1
    (lambda (n)
      (+ n 1)))

  (define sub1
    (lambda (n)
      (- n 1)))

  (define atom?
    (lambda (s)
      (and (not (pair? s))
           (not (null? s)))))
  )
