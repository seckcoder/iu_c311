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

  ; 2.2
  ; I'm not sure what's the usage of unary representation.
  ; For the scheme number representation, this is more efficient
  ; when used for add/subtract(mathematical operation).
  ; For the bignum representation, it saves space when used to store bignum.
  ;

  ; 2.3
  ; 2.3.1. every number: n = n + 0 = diff(n, 0). 0 has infinitely many
  ; representations in the system

  ; 2.3.2

  (define difftree-one '(one))
  (define difftree-is-one?
    (lambda (t)
      (eq? t difftree-one)))
  (define difftree-left cadr)
  (define difftree-right caddr)
  (define difftree-diff
    (lambda (left right)
      (list 'diff left right)))

  (define difftree/zero
    (lambda ()
      (difftree-diff difftree-one difftree-one)))

  (define difftree/one
    (lambda ()
      difftree-one))
  (define difftree/-one
    (lambda ()
      (difftree-diff (difftree/zero) (difftree/one))))

  (define difftree/is-zero?
    (lambda (n)
      (= (difftree->natural n) 0)))

  (define difftree/predecessor
    (lambda (n)
      (difftree-diff n (difftree/one))))

  (define difftree/successor
    (lambda (n)
      (difftree-diff n (difftree/-one))))

  (define difftree->natural
    (lambda (n)
      (if (difftree-is-one? n)
        1
        (- (difftree->natural (difftree-left n))
           (difftree->natural (difftree-right n))))))

  ; 2.3.3
  ; TODO(liwei): I'm not sure what optimized means.

  (define empty-env
    (lambda () (list 'empty-env)))

  (define extend-env
    (lambda (var val env)
      (list 'extend-env var val env)))

  (define extend-env?
    (lambda (env)
      (eq? (car env) 'extend-env)))

  (define apply-env
    (lambda (env search-var)
      (cond
        ((empty-env? env)
         (error 'apply-env "no binding for ~s" search-var))
        ((extend-env? env)
         (let ((saved-var (cadr env))
               (saved-val (caddr env))
               (saved-env (cadddr env)))
           (if (eq? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
        (else
          (error 'apply-env "env:~s format is not right" env)))))
  ; 2.4
  ; specification
  ;   (empty-stack) = [null] (constructor)
  ;   (push [f] v) = [g]  (constructor)
  ;   (pop [f]) = [g]  (observer?)
  ;   (top [f]) = v (observer)
  ;   (empty-stack? [f]) = t/f  (observer)
  ;
  ; 2.5...

  ; 2.6
  ; use hash-table, association list, etc ...
  ; As I've implemented one using hash-table for sicp exercise, I'll ignore this one

  ; 2.7 ...

  ; 2.8
  (define empty-env?
    (lambda (env)
      (eq? (car env) 'empty-env)))
  ; 2.9
  ; almost the same as apply-env

  ; 2.10
  (define extend-env*
    (lambda (vars vals env)
      (cond ((and (null? vars)
                  (null? vals))
             env)
            ((or (null? vars)
                 (null? vals))
             (error 'extend-env* "vars and vals supplied is not of equal length"))
            (else
              (extend-env (car vars)
                          (car vals)
                          (extend-env* (cdr vars)
                                       (cdr vals)
                                       env))))))

  ; 2.11
  ; solution is obvious.
  
  (define procedural/empty-env
    (lambda ()
      (lambda (search-var)
        (error 'apply-env "no binding found for ~s" search-var))))

  (define procedural/extend-env
    (lambda (saved-var saved-val saved-env)
      (lambda (search-var)
        (if (eq? search-var saved-var)
          saved-val
          (procedural/apply-env saved-env search-var)))))

  (define procedural/apply-env
    (lambda (env search-var)
      (env search-var)))

  ; 2.12 procedural representation for stack

  ; following is data structure representation(using list)
  ; for procedural representation see c2-stack.scm
  (define empty-stack
    (lambda ()
      '()))

  (define empty-stack?
    (lambda (st)
      (null? st)))

  (define pop
    (lambda (st)
      (cdr st)))

  (define push
    (lambda (st v)
      (cons v st)))

  (define top
    (lambda (st)
      (car st)))

  ; 2.13
  ;
  ; 2.14 trivial
  ;
  ; for another example of procedural representation, see lisp.js in my
  ; sicp exercise.
  ;
  ; There is a discussion on the problem on eopl's google group:
  ; https://groups.google.com/forum/#!topic/eopl3/Wf5nbWjhwKE

  ; chapter 2, section 3: interfaces for recursive data types
  ; 2.15 in eopl/c1.scm
  ;
  ; 2.16 in c2-16-lambda-exp.scm
  ;
  ; 2.17
  ; There are many different kinds of lambda exp in languages such as
  ; js, haskell...
  ; 
  ; 2.18
  (define number->sequence
    (lambda (n)
      (list n '() '())))
  (define current-element
    (lambda (biseq)
      (car biseq)))
  (define biseq->left
    (lambda (biseq)
      (cadr biseq)))
  (define biseq->right
    (lambda (biseq)
      (caddr biseq)))
  (define move-to-left
    (lambda (biseq)
      (let ((right-lst (biseq->right biseq)))
        (if (null? right-lst)
          (error 'move-to-left "at the right end of sequence")
          (list (car right-lst)
                (cons (current-element biseq)
                      (biseq->left biseq))
                (cdr right-lst))))))
  (define move-to-right
    (lambda (biseq)
      (let ((left-lst (biseq->left biseq)))
        (if (null? left-lst)
          (error 'move-to-right "at the left end of sequence")
          (list (car left-lst)
                (cdr left-lst)
                (cons (current-element biseq)
                      (biseq->right biseq)))))))
  (define insert-to-left
    (lambda (n biseq)
      (list (current-element biseq)
            (cons n (biseq->left biseq))
            (biseq->right biseq))))
  (define insert-to-right
    (lambda (n biseq)
      (list (current-element biseq)
            (biseq->left biseq)
            (cons n (biseq->right biseq)))))

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
        ))
    (difftree
      (lambda (fail)
        (or (equal? (difftree/is-zero?
                      (difftree/successor
                        (difftree/predecessor
                          (difftree/successor
                            (difftree/predecessor (difftree/zero))))))
                    #t)
            (fail))
        ))

    (precedural/stack
      (lambda (fail)
        (let ((st (empty-stack)))
          (or (equal? (top (pop (push (push (push st 3) 4) 5)))
                      4)
              (fail)))))
    (bidirectional-sequences
      (lambda (fail)
        (let ((biseq (number->sequence 7)))
          (or (equal? (current-element biseq) 7) (fail))
          (let ((biseq (move-to-right (insert-to-left 13 biseq))))
            (or (equal? (current-element biseq) 13) (fail))
            (let ((biseq (move-to-left (insert-to-right 12 biseq))))
              (or (equal? (current-element biseq) 12) (fail)))))))
    )
  )
