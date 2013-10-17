(library
  (eopl c2)
  (export eopl-c2
          bignum/successor
          bignum/predecessor
          bignum/fact
          natural->bignum
          bignum->natural
          bignum/mul
          )

  (import (rnrs)
          (rnrs r5rs)
          (elegant-weapons tester)
          (elegant-weapons compat)
          (utils)
          )

  ; TODO(liwei): use macro
  (define unary/zero
    (lambda () '()))
  (define unary/is-zero?
    (lambda (n) (null? n)))
  (define unary/successor
    (lambda (n) (cons #t n)))
  (define unary/predecessor
    (lambda (n) (cdr n)))

  ; 2.1
  ; my implementation will cause stack-overflow when number is greater than 9...
  ; When argument is greater, the execution time is also greater.
  ; But it seems to be of little relation with the base. That's mainly because
  ; we have to use the bignum/successor or predecessor when we do add/mul, which
  ; will cause great stack usage for recursion.
  (define bignum/base 16)
  (define bignum/zero
    (lambda () '()))
  (define bignum/one
    (lambda () '(1)))
  (define bignum/is-zero?
    (lambda (n) (null? n)))
  (define bignum/is-one?
    (lambda (n)
      (and (not (bignum/is-zero? n))
           (= (car n) 1)
           (bignum/is-zero? (cdr n)))))

  (define bignum/successor
    (lambda (n)
      (if (bignum/is-zero? n)
        (cons 1 n)
        (let ((lst-bigit (car n)))
          (if (= (add1 lst-bigit)
                 bignum/base)
            (cons 0
                  (bignum/successor (cdr n)))
            (cons (add1 lst-bigit)
                  (cdr n)))))))

  (define bignum/predecessor
    (lambda (n)
      (if (bignum/is-zero? n)
        (error 'bignum/predecessor "undefined predecessor for 0")
        (let ((lst-bigit (car n)))
          (cond ((zero? lst-bigit)
                 (cons
                   (sub1 bignum/base)
                   (bignum/predecessor (cdr n))))
                ((bignum/is-one? n)
                 (bignum/zero))
                (else
                  (cons
                    (sub1 lst-bigit)
                    (cdr n))))))))

  (define bignum/cmp
    (lambda (a b)
      (let loop ((rev-a (reverse a))
                 (rev-b (reverse b)))
        (cond ((and (null? rev-a)
                    (null? rev-b)) 'equal)
              ((null? rev-a) 'less)
              ((null? rev-b) 'greater)
              ((< (car rev-a)
                  (car rev-b)) 'less)
              ((> (car rev-a)
                  (car rev-b)) 'greater)
              ((= (car rev-a)
                  (car rev-b))
               (loop (cdr rev-a)
                     (cdr rev-b)))))))

  (define bignum/<
    (lambda (a b)
      (eq? (bignum/cmp a b)
           'less)))

  (define bignum/<=
    (lambda (a b)
      (not (bignum/> a b))))

  (define bignum/>
    (lambda (a b)
      (eq? (bignum/cmp a b)
           'greater)))

  (define bignum/add
    (lambda (a b)
      (cond ((bignum/is-zero? a) b)
            ((bignum/<= a b)
             (bignum/successor (bignum/add (bignum/predecessor a)
                                           b)))
            (else
              (bignum/add b a)))))

  (define bignum/mul
    (lambda (a b)
      (cond ((bignum/is-zero? a)
             (bignum/zero))
            ((bignum/<= a b)
             (bignum/add b
                         (bignum/mul (bignum/predecessor a)
                                     b)))
            (else
              (bignum/mul b a)))))

  (define natural->bignum
    (lambda (n)
      (if (= n 0)
        (bignum/zero)
        (cons (remainder n bignum/base)
              (natural->bignum (quotient n bignum/base))))))

  (define bignum->natural
    (lambda (n)
      (let loop ((bignum n)
                 (multipler 1))
        (if (bignum/is-zero? bignum)
          0
          (+ (* multipler
                (car bignum))
             (loop (cdr bignum)
                   (* multipler
                      bignum/base)))))))

  (define bignum/fact
    (lambda (n)
      (let loop ((accum (bignum/one))
                 (n n))
        (if (bignum/is-zero? n)
          accum
          (loop (bignum/mul n accum)
                (bignum/predecessor n))))))

  (define-test-suite
    eopl-c2
    (bignum
      (lambda (fail)
        (or (equal? (bignum/is-zero? (bignum/zero)) #t) (fail))
        (or (equal? (bignum/successor (list (sub1 bignum/base)))
                    '(0 1))
            (fail))
        (or (equal? (bignum/predecessor '(0 1))
                    (list (sub1 bignum/base)))
            (fail))
        (or (equal? (bignum/successor (list (sub1 bignum/base)
                                            (sub1 bignum/base)))
                    '(0 0 1))
            (fail))
        (or (equal? (bignum/predecessor '(0 0 1))
                    (list (sub1 bignum/base)
                          (sub1 bignum/base)))
            (fail))
        (or (equal? (natural->bignum 0) (bignum/zero)) (fail))
        (or (equal? (natural->bignum bignum/base) '(0 1)) (fail))
        (or (equal? (bignum->natural '(0 1)) bignum/base) (fail))
        (or (equal? (bignum/add (natural->bignum (* 3 bignum/base))
                                (natural->bignum bignum/base))
                    (natural->bignum (* 4 bignum/base)))
            (fail))
        (or (equal? (bignum/mul (natural->bignum 43)
                                (natural->bignum 55))
                    (natural->bignum (* 43 55)))
            (fail))
        )))
  )
