(run 2 (q)
  (all
    alwayso
    (conde
      ((== #f q))
      ((== #t q)))
    )
  (== #t q))

(run 1 (q)
  (all
    (conde
      ((== #f q))
      ((== #t q)))
    ; this just hangs here.
    ; This demo shows that all tries to exhaust the bottom line first
    alwayso
    )
  (== #t q))

(run 1 (q)
  (alli
    ; all interleave. When one fails, it enumerates conde instead of
    ; just alwayso
    (conde
      ((== #f q))
      ((== #t q)))
    alwayso
    )
  (== #t q))


(run 5 (q)
  (all
    (conde
      (succeed)
      (nevero))
    alwayso)
  (== #t q))

(run 5 (q)
  (alli
    (conde
      (succeed)
      (nevero))
    alwayso)
  (== #t q))

; sincerely, I still don't quit get all and alli...
