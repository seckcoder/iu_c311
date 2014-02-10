(add-tests-with-string-output "add1"
  [(add1 0) => "1\n"]
  [(add1 -1) => "0\n"]
  [(add1 1) => "2\n"]
  [(add1 -100) => "-99\n"]
  [(add1 1000) => "1001\n"]
  [(add1 536870910) => "536870911\n"]
  [(add1 -536870912) => "-536870911\n"]
  [(add1 (add1 0)) => "2\n"]
  [(add1 (add1 (add1 (add1 (add1 (add1 12)))))) => "18\n"]
  )

(add-tests-with-string-output "number->char and char->number"
   [(number->char 65) => "#\\A\n"]
   [(number->char 97) => "#\\a\n"]
   [(number->char 122) => "#\\z\n"]
   [(number->char 90) => "#\\Z\n"]
   [(number->char 48) => "#\\0\n"]
   [(number->char 57) => "#\\9\n"]
   [(char->number #\A) => "65\n"]
   [(char->number #\a) => "97\n"]
   [(char->number #\z) => "122\n"]
   [(char->number #\Z) => "90\n"]
   [(char->number #\0) => "48\n"]
   [(char->number #\9) => "57\n"]
   [(char->number (number->char 12)) => "12\n"]
   [(number->char (char->number #\x)) => "#\\x\n"]
)

(add-tests-with-string-output "number?"
   [(number? 0) => "#t\n"]
   [(number? 1) => "#t\n"]
   [(number? -1) => "#t\n"]
   [(number? 37287) => "#t\n"]
   [(number? -23873) => "#t\n"]
   [(number? 536870911) => "#t\n"]
   [(number? -536870912) => "#t\n"]
   [(number? #t) => "#f\n"]
   [(number? #f) => "#f\n"]
   [(number? ()) => "#f\n"]
   [(number? #\Q) => "#f\n"]
   [(number? (number? 12)) => "#f\n"]
   [(number? (number? #f)) => "#f\n"]
   [(number? (number? #\A)) => "#f\n"]
   [(number? (char->number #\r)) => "#t\n"]
   [(number? (number->char 12)) => "#f\n"]
)


(add-tests-with-string-output "zero?"
   [(zero? 0) => "#t\n"]
   [(zero? 1) => "#f\n"]
   [(zero? -1) => "#f\n"]
)

(add-tests-with-string-output "null?"
   [(null? ()) => "#t\n"]
   [(null? #f) => "#f\n"]
   [(null? #t) => "#f\n"]
   [(null? (null? ())) => "#f\n"]
   [(null? #\a) => "#f\n"]
   [(null? 0) => "#f\n"]
   [(null? -10) => "#f\n"]
   [(null? 10) => "#f\n"]
)

(add-tests-with-string-output "boolean?"
   [(boolean? #t) => "#t\n"]
   [(boolean? #f) => "#t\n"]
   [(boolean? 0) => "#f\n"]
   [(boolean? 1) => "#f\n"]
   [(boolean? -1) => "#f\n"]
   [(boolean? ()) => "#f\n"]
   [(boolean? #\a) => "#f\n"]
   [(boolean? (boolean? 0)) => "#t\n"]
   [(boolean? (fixnum? (boolean? 0))) => "#t\n"]
)

(add-tests-with-string-output "char?"
   [(char? #\a) => "#t\n"]
   [(char? #\Z) => "#t\n"]
   [(char? #\newline) => "#t\n"]
   [(char? #t) => "#f\n"]
   [(char? #f) => "#f\n"]
   [(char? ()) => "#f\n"]
   [(char? (char? #t)) => "#f\n"]
   [(char? 0) => "#f\n"]
   [(char? 23870) => "#f\n"]
   [(char? -23789) => "#f\n"]
)

(add-tests-with-string-output "not"
  [(not #t) => "#f\n"]
  [(not #f) => "#t\n"]
  [(not 15) => "#f\n"]
  [(not ()) => "#f\n"]
  [(not #\A) => "#f\n"]
  [(not (not #t)) => "#t\n"]
  [(not (not #f)) => "#f\n"]
  [(not (not 15)) => "#t\n"]
  [(not (fixnum? 15)) => "#f\n"]
  [(not (fixnum? #f)) => "#t\n"]
)

#|(add-tests-with-string-output "fxlognot"
 [($fxlognot 0) => "-1\n"]
 [($fxlognot -1) => "0\n"]
 [($fxlognot 1) => "-2\n"]
 [($fxlognot -2) => "1\n"]
 [($fxlognot 536870911) => "-536870912\n"]
 [($fxlognot -536870912) => "536870911\n"]
 [($fxlognot ($fxlognot 237463)) => "237463\n"]
)|#

