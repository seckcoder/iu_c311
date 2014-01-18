#lang racket

(provide (all-defined-out))

(struct Production (unterm prod es))

(define (terminal? v)
  (or (string? v)
      (memq v '(sigma $ number))))

(define (make-prod prod)
  (match prod
    [(list unterm prod es)
     (Production unterm prod es)]))

(define (print-table tbl)
  (for-each
    (match-lambda
      [(cons k v)
       (printf "~a ~a\n" k v)])
    (hash->list tbl)))

(define (hash-union hs k s)
  (hash-set hs
            k
            (set-union (hash-ref hs k (set))
                       s)))

(define (empty-table)
  (make-immutable-hash))

(struct tblkey2d (k1 k2)
        #:transparent)

(define (extend-table2d tbl k1 k2 v)
  (hash-set tbl (tblkey2d k1 k2) v))

(define (apply-table2d tbl k1 k2)
  (hash-ref tbl (tblkey2d k1 k2)))
