#lang racket
; arithmetic operation grammar

(provide arithmetic-prods
         simple-prods)

(define arithmetic-prods
  '((E E1 (T X))
    (X X1 ("+" E))
    (X X2 (sigma))
    (T T1 ("(" E ")"))
    (T T2 (number Y))
    (Y Y1 ("*" T))
    (Y Y2 (sigma))))

(define simple-prods
  '((E E1 (T X))
    (T T1 (number))
    (T T2 (sigma))
    (X X1 ("+"))))
