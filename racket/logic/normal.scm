(define flatten
  (lambda (s)
    (cond
      ((null? s) '())
      ((pair? s)
       (append
         (flatten (car s))
         (flatten (cdr s))))
      (else
        (cons s '())))))
