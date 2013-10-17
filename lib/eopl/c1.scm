(library
  (eopl c1)
  (export occurs-free?
          eopl-c1)
  (import (rnrs)
          (elegant-weapons tester)
          )

  (define occurs-free?
    (lambda (var lcexp)
      (cond ((identifier? lcexp)
             (eq? var lcexp))
            ((lambda-exp? lcexp)
             (and (not (eq? var (lambda-binding lcexp)))
                  (occurs-free? var (lambda-body lcexp))))
            (else
              (or (occurs-free? var (lcexp-first lcexp))
                  (occurs-free? var (lcexp-second lcexp)))))))

  (define identifier? symbol?)
  (define lambda-exp?
    (lambda (lcexp)
      (eq? (car lcexp)
           'lambda)))
  (define lambda-binding
    (lambda (lcexp)
      (caadr lcexp)))
  (define lambda-body
    (lambda (lcexp)
      (caddr lcexp)))

  (define lcexp-first car)
  (define lcexp-second cadr)

  (define-test-suite
    eopl-c1
    (occurs-free
      (lambda (fail)
        (or (equal? (occurs-free? 'x 'x) #t) (fail))
        (or (equal? (occurs-free? 'x 'y) #f) (fail))
        (or (equal? (occurs-free? 'x '(lambda (x) (x y))) #f) (fail))
        (or (equal? (occurs-free? 'x '(lambda (y) (x y))) #t) (fail))
        (or (equal? (occurs-free? 'x '((lambda (x) x) (x y))) #t) (fail))
        (or (equal? (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))) #t) (fail))
        )))
  )
