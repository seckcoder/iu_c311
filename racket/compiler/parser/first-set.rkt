#lang racket

(require racket/set
         "ds.rkt"
         "demos.rkt")

(provide first-table
         first-set-of-e
         first-set-of-es)

; full of side effect
(define (first-table prods)
  (define (get-prods unterm)
    (filter (lambda (prod)
              (eq? (Production-unterm prod)
                   unterm))
            prods))
  (define (from-prods prods tbl)
    (cond
      ((null? prods) tbl)
      (else
        (from-prods (cdr prods)
                    (match (car prods)
                      [(Production unterm prod-n es)
                       (from-es unterm es tbl)])))))
  (define (from-es unterm es tbl)
    (cond
      ((null? es)
       ; A -> B1B2B3...Bn and 'sigma (- B1,B2,...Bn
       (hash-union tbl unterm (set 'sigma)))
      ((terminal? (car es))
       ; A -> tB or A -> B1B2...tB... and sigma (- B1B2...
       (hash-union tbl unterm (set (car es))))
      (else
        ; A -> B1B2, 
        ; first(B1) belong first(A)
        ; TODO: add detection for left-recursive?
        (let* ((e0 (car es))
               (tbl (from-unterm e0 tbl))
               (e0-set (hash-ref tbl e0)))
          (let ((tbl (hash-union tbl unterm e0-set)))
            (if (set-member? (hash-ref tbl (car es))
                           'sigma)
              (from-es unterm (cdr es) tbl)
              tbl))))))
  (define (from-unterm unterm tbl)
    (let ((prods (get-prods unterm)))
      (from-prods prods tbl)))
  (from-prods prods (make-immutable-hasheq))
  )

(define (first-set-of-e first-table e)
  (if (terminal? e)
    (set e)
    (hash-ref first-table e (set))))

; handle-end: A = B1B2B3, B1->sigma and B2->sigma and B3->sigma
(define (first-set-of-es first-table es handle-end)
  (let loop ((es es)
             (fst-set (set)))
    (cond
      ((null? es)
       (handle-end fst-set))
      (else
        (let ((e-fst-set (first-set-of-e first-table (car es))))
          (if (set-member? e-fst-set 'sigma)
            (loop (cdr es)
                  (set-union fst-set e-fst-set))
            (set-union fst-set e-fst-set)))))))

(module+ test
  (require rackunit)
  (first-table (map make-prod arithmetic-prods))
  ;(first-set (map make-prod simple-prods))
  )
