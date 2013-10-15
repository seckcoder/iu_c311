(import (rnrs)
        (elegant-weapons compat))

(define countdown
  (lambda (n)
    (if (= n 0)
      '(0)
      (cons n (countdown (sub1 n))))))

(define insertR
  (lambda (old new lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons old
                 (cons new
                       (insertR old new (cdr lat)))))
          (else
            (cons (car lat)
                  (insertR old new (cdr lat)))))))

(define remv-lst
  (lambda (sym lat)
    (cond ((null? lat) '())
          ((eq? sym (car lat))
           (cdr lat))
          (else
            (cons (car lat)
                  (remv-lst sym (cdr lat)))))))

(define occurs-s-num
  (lambda (sym lat)
    (cond ((null? lat) 0)
          ((eq? sym (car lat))
           (add1 (occurs-s-num sym (cdr lat))))
          (else
            (occurs-s-num sym (cdr lat))))))

(define occurs-?s
  (lambda (lat)
    (occurs-s-num '? lat)))

; filter ...

(define zip
  (lambda (lst1 lst2)
    (cond ((and (null? lst1)
                (null? lst2))
           '())
          ((or (null? lst1)
               (null? lst2))
           (error 'zip "list args should have the same length"))
          (else
            (cons (cons (car lst1)
                        (car lst2))
                  (zip (cdr lst1)
                       (cdr lst2)))))))


; map ...

; append ...

; reverse ...

; fact ...

(define member*
  (lambda (sym lst)
    (cond ((null? lst) #f)
          ((atom? (car lst))
           (or (eq? sym
                    (car lst))
               (member* sym (cdr lst))))
          (else
            (or (member* sym (car lst))
                (member* sym (cdr lst)))))))

(define member-?*
  (lambda (lst)
    (member* '? lst)))
