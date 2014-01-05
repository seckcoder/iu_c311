#lang racket
(require "../base/utils.rkt")

; Demo of list monad

; list monad(for backtracking)
(define unit
  (lambda (a)
    `(,a)))

(define bind
  (lambda (ma f)
    ; (f ma)->mb
    ; flatten/append list of mb:(b1 b2 b3 ...)
    (flatmap f ma)))

; failed
(define mzero '())

; combine multi results
(define mplus append)

; Example: parsing ambiguous grammars.


(define char-hex?
  (lambda (c)
    ; c:[0-10,a-f]
    (or (char-numeric? c)
        (char<=? #\a c #\f))))

(define char-hex->integer/safe
  (lambda (c)
    (- (char->integer c)
       (if (char-numeric? c)
         (char->integer #\0)
         (- (char->integer #\a) 10)))))

(define parse-hex-digit
  (lambda (a c)
    (cond
      ((and (eq? (car a) 'hex-num)
            (char-hex? c))
       (unit `(hex-num . ,(+ (* (cdr a) 16) (char-hex->integer/safe c)))))
      (else mzero))))

(define parse-dec-digit
  (lambda (a c)
    (cond
      ((and (eq? (car a) 'dec-num)
            (char-numeric? c))
       (unit `(dec-num . ,(+ (* (cdr a) 10) (char->integer c)))))
      (else mzero))))

(define parse-alphanumeric
  (lambda (a c)
    (cond
      ((and (eq? (car a) 'word-string)
            (or (char-alphabetic? c)
                (char-numeric? c)))
       (unit `(word-string . ,(string-append (cdr a)
                                             (string c)))))
      (else mzero))))

(define parse-c*
  (lambda (a c*)
    (cond
      ((null? c*) (unit a))
      (else
        (bind (mplus
                (parse-hex-digit a (car c*))
                (parse-dec-digit a (car c*))
                (parse-alphanumeric a (car c*)))
              (lambda (a)
                (parse-c* a (cdr c*))))))))

(define parse
  (lambda (c*)
    (bind (mplus
            (unit '(hex-num . 0)) 
            (unit '(dec-num . 0))
            (unit '(word-string . "")))
          (lambda (a)
            (parse-c* a c*)))))

(define parse-str
  (lambda (s)
    (parse (string->list s))))

(parse-str "abc")
(parse-str "10")
(parse-str "g0")


; TODO other example of list monad?
