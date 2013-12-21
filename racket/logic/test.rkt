#lang racket
(require "smk.rkt"
         "utils.scm"
         "../base/utils.rkt")

(module+ test
  (check equal? (run 2 (x)
                  (teacupo x))
                '(tea cup))

  (check equal? (run 1 (v)
                  (conde
                    ((== v #f))
                    ((== v #t)))
                  (== v #t))
                '(#t))

  (check equal? ((all
                   fail) empty-s)
                '())
  (check equal?
    (run* (q)
      fail)
    '())

  (check equal?
    (run* (x)
      (let ((x #f))
        (== #t x)))
    '())

  (check equal?
         (run* (l)
           (conso '(a b c) '(d e) l))
         '(((a b c) d e)))

  (check equal?
         (run* (x)
           (conso x '(a b c) '(d a b c)))
         '(d))

  
  (check equal? 
         (run 1 (x)
           (fresh (l)
             (== `(d a ,x c) l)
             (conso x `(a ,x c) l)))
         '(d))

  (check equal?
         (run* (x)
           (listo `(a b ,x d)))
         '(_.0))

  (check equal?
         (run 5 (x)
           (listo `(a b c . ,x)))
         '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3)))

  )
