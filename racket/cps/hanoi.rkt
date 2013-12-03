#lang eopl

(define-datatype
  continuation continuation?
  (endcont)
  (move1-cont
    (n number?)
    (from symbol?)
    (to symbol?)
    (by symbol?)
    (saved-cont continuation?))
  (move2-cont
    (n number?)
    (from symbol?)
    (to symbol?)
    (by symbol?)
    (saved-cont continuation?)))

#|(define move
  (lambda (n from to by)
    (if (= n 1)
      'finish
      (begin
        (move (- n 1) from by to)
        (move 1 from to by)
        (move (- n 1) by to from)))))|#

(define move
  (lambda (n from to by)
    (move/k n from to by (endcont))))

(define move/k
  (lambda (n from to by cont)
    (if (= n 1)
      (begin
        (eopl:printf "move ~s from:~s to:~s by:~s" n from to by)
        (newline)
        (apply-cont cont))
      (begin
        (move/k (- n 1)
                from
                by
                to
                (move1-cont n from to by cont))))))

(define apply-cont
  (lambda (cont)
    (cases continuation cont
      (endcont
        ()
        'ok)
      (move1-cont
        (n from to by saved-cont)
        (move/k 1
                from
                to
                by
                (move2-cont n from to by saved-cont)))
      (move2-cont
        (n from to by saved-cont)
        (move/k (- n 1) by to from saved-cont)))))

(move 4 'a 'b 'c)
