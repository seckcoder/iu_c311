#lang racket
(require "../base/utils.rkt")

; contract num
(define num
  (lambda (v)
    (if (number? v)
      v
      (error "num contract"))))

; contract str
(define str
  (lambda (v)
    (if (string? v)
      v
      (error "str contract"))))

; morphism: str -> num
(define len-of-s
  (lambda (s)
    (num (string-length (str s)))))

(define lst
  (lambda (v)
    (if (list? v)
      v
      (error "list contract"))))


; listof Functor
(define listof
  (lambda (c)
    ; A new contract: listof-c
    (lambda (v)
      (map c (lst v)))))

; contract -> contract
; ((listof num) '(1 2 3))
; morphism -> morphism
; ((listof len-of-s) '("abc" "def"))

; contracts + morphisms(composable) = category

; Maybe Functor
(define maybe
  (lambda (c)
    ; New contract: null | (c);
    (lambda (v)
      (cond ((null? v)
             v)
            ((list? v)
              ; unwrap c from v and wrap again
             (list (c (car v))))
            (else
              (error "contract for maybe violated")
              )))))

; contract: maybe a number or null
(define maybe-num (maybe num))
; (maybe-num '(1))
; (maybe-num '())

; a new morphism
(define len-of-s-or-null (maybe len-of-s))
; ((maybe len-of-s) '("abcd"))
; ((maybe len-of-s) '())


; unit and flatten

; unit is a functor
; A unit for listof
(define listof-unit
  (lambda (c)
    (lambda (x)
      ; wrap with list(you can wrap it with anything
      ((listof c) (list (c x))))))

; ((listof-unit num) 3)

(define maybe-unit
  (lambda (c)
    (lambda (x)
      #|(let ((x (((notimes maybe) c) x)))
          (((once maybe) c))(list x)))))|#
      (let ((x (c x))) ; no times
        ((maybe c) (list x)))))) ; once
      


; What's unit?
; (unit int) -> (wrap int) : (list int) | (class int) ...
; just wrap something

; ((maybe-unit num) 4)

; listof-flatten is a functor
; it generate a function that will flatten
; a list of a listof c
(define listof-flatten
  (lambda (c)
    (lambda (aax)
      (let ((x (((twice listof) c) aax))) ; twice
        ((listof c) (flatmap (lambda (v) v) x)))))) ; once

; ((listof-flatten num) '((1 2) (3 4)))


(define maybe-flatten
  (lambda (c)
    (lambda (mmx)
      (let ((mmx (((twice maybe) c) mmx))) ; twice
        (cond ((null? mmx) mmx) ; notimes
              ((list? mmx)
               ((maybe c) (car mmx)))))))) ; once

((maybe-flatten num) '((3)))
; contract violated
; ((maybe-flatten num) '(3))
; ((maybe-flatten num) '(()))


; For a functor, it generates a morphism by a contract:c1
; For the morphism, it accepts something that follow a contract:c2 and returns
; something else follow a contract:c3.
 
; Generate a functor that will apply another functor twice
(define twice
  (lambda (functor)
    (lambda (c)
      (functor (functor c)))))

; Generate a functor that will apply another functor once
(define once
  (lambda (functor)
    (lambda (c)
      (functor c))))

(define notimes
  (lambda (functor)
    (lambda (c)
      c)))


; Monad

; why do we need monad?
;   - easy to debug
;   - construct monad that's not builtin language features but as easy to use as the builtin.(Make libraries easy to use).
