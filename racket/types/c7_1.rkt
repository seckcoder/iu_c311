(lambda (x) (- x 3))
-> (int -> int)

(lambda (f)
  (lambda (x)
    (- (f x) 1)))

-> ((t -> int) -> (t -> int))

(lambda (x) x)
-> (t -> t)
(lambda (x)
  (lambda (y)
    (x y)))

-> ((t -> t) -> (t -> t))

(lambda (x)
  (x 3))

-> ((int -> t) -> t)

(lambda (x)
  (x x))

; How to reprent x?
-> (((any proc->t) -> t) -> t)

(lambda (x)
  (if x
    88
    99))

(lambda (x)
  (lambda (y)
    (if x
      y
      99)))

((lambda (p)
   (if p
     88
     99))
 33)

((lambda (p)
   (if p
     88
     99))
   33)

((lambda (p)
   (if p
     88
     99))
 (lambda (z) z))

; 70
; No.

; 73
(define (foo x)
  x)
; You can't decide the type of x.
; Therefore, for some cases you can decide.
; However, if you can't decide the type of some value,
; then this value is just declared but without giving a type.
; What's more, it's not referenced. So we can just delete it!
; So for the smallest subset of a code written in letrec, we can decide 
; the type of any expressed value.
