#lang eopl

(define fact
  (lambda (n)
    ((lambda (mk)
       ((mk mk) n))
     (lambda (mk)
       (lambda (n)
         (if (= n 0)
           1
           (* n ((mk mk) (- n 1))))
         ))))
  )

(lambda (n k)



; version1
(define fact
  (lambda (n)
    ((lambda (mk k)
       (mk mk (lambda (fact)
                (fact n (lambda (v)
                          (k v))))))
     (lambda (mk k1)
       (k1
         (lambda (n k2)
           (if (= n 0)
             (k2 1)
             (mk mk (lambda (fact)
                      (fact (- n 1) (lambda (v)
                                      (k2 (* n v))))))))))
     (lambda (v)
       v)
     )))

; version 2(This involves more complex analysis)
(define fact
  (lambda (n)
    ((lambda (mk)
       (mk mk (lambda (fact/k)
                (fact/k n (lambda (v)
                            v)))))
     (lambda (mk k)
       (k
         (lambda (n k)
           (if (= n 0)
             (k 1)
             (mk mk (lambda (fact/k)
                      (fact/k (- n 1) (lambda (v)
                                        (k (* n v))))))))))
     ))
  )

(display (fact 10))
