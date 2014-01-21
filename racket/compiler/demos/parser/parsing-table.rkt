#lang racket


(require "ds.rkt"
         "first-set.rkt"
         "follow-set.rkt"
         "demos.rkt")

(provide build-parsing-table)

(define (build-parsing-table-from-fsets prods fst-table folo-table)
  (foldl
    (lambda (prod parsing-table)
      (match prod
        [(Production unterm prod-n es)
         (let* ((f-set (first-set-of-es fst-table
                                        es
                                        (lambda (fst-set)
                                          (set-union fst-set
                                                     (hash-ref folo-table
                                                               unterm
                                                               (set))
                                                     fst-set)
                                          )))
                (terms (set->list f-set)))
           (foldl
             (lambda (term parsing-table)
               (extend-table2d
                 parsing-table
                 unterm
                 term
                 es))
             parsing-table
             terms))]))
    (empty-table)
    prods))

(define (build-parsing-table prods)
  (let* ((prods (map make-prod arithmetic-prods))
         (fst-table (first-table prods))
         (folo-table (follow-table prods fst-table)))
    (build-parsing-table-from-fsets prods fst-table folo-table)))


(module+ test
  (build-parsing-table (map make-prod arithmetic-prods))
  )
