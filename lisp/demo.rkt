(let ((a 1))
  ((let ((a 2))
     (lambda (b)
       (list a b)))
   3))

((lambda (a)
   (((lambda (a)
      (lambda (b)
        (list a b))) 2)
    3)
   )
 1)


(let ((x (lambda () 1)))
  (x))

((let ((x (lambda () 1)))
   x))
