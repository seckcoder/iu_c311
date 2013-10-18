(library
  (eopl c1)
  (export occurs-free?
          eopl-c1)
  (import (rnrs)
          (elegant-weapons tester)
          )

  (define occurs-free?
    (lambda (var lcexp)
      (cond ((var-exp? lcexp)
             (eq? var lcexp))
            ((lambda-exp? lcexp)
             (and (not (eq? var (lambda-exp->bound-var lcexp)))
                  (occurs-free? var (lambda-exp->body lcexp))))
            (else
              (or (occurs-free? var (app-exp->ractor lcexp))
                  (occurs-free? var (app-exp->rand lcexp)))))))

  (define var-exp? symbol?)
  (define lambda-exp?
    (lambda (lcexp)
      (eq? (car lcexp)
           'lambda)))
  (define lambda-exp->bound-var
    (lambda (lcexp)
      (caadr lcexp)))
  (define lambda-exp->body
    (lambda (lcexp)
      (caddr lcexp)))

  (define app-exp->ractor car)
  (define app-exp->rand cadr)

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
