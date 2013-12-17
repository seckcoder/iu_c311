(define anyo
  (lambda (g)
    (conde
      (g)
      (else (anyo g)))))

(define nevero (anyo fail))
(define alwayso (anyo succeed))

; demo 1 to understand nevero

(run 1 (q)
  nevero
  (== #t q))

(run 1 (q)
  fail
  (== #t q))

; demo 2 to understand always

(run* (q)
  always
  (== #t q))

(run* (q)
  succeed
  (== #t q))

; demo 3 to understand condi

(run 2 (q)
  (conde
    ((== #f q)
     ; this demo hangs at this line since conde
     ; will exhaust the successful goals in the line
     alwayso)
    ((== #t q) alwayso))
  (== #t q))

(run 2 (q)
  (condi
    ((== #f q)
     ; when the line failed for once, it tries the second line
     alwayso)
    ((== #t q) alwayso))
  (== #t q))

; a further demo

(run 2 (q)
  (condi
    ((== #t q)
     alwayso)
    ; This demo will hangs at this line. After trying the above
    ; line and get an answer, it tries this line. But this line
    ; just fails to do so. What's worse, it never gives up.
    (nevero))
  (== #t q))

(run 2 (q)
  ; So this time, conde wins
  (conde
    ((== #t q)
     alwayso)
    (nevero))
  (== #t q))
