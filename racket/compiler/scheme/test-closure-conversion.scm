#|(check equal? (free '(lambda (x) (+ x y))) (seteq 'y))
(check equal?
       (free '(lambda ()
                (lambda ()
                  (g x y))))
       (seteq 'g 'x 'y))
(check equal?
       (subst '(lambda (x) (+ x y))
              'env
              (seteq 'y))
       '(lambda (x) (+ x (env-ref env y))))
|#

(pretty-write
  (closure-conversion
    '(lambda (x)
       (+ x a))
    'bottom-up))
(newline)

(pretty-write
  (closure-conversion
    '(app f x)
    'bottom-up))
(newline)

(pretty-write
  (closure-conversion
    '(app
       (lambda (x)
        (+ x y)) u)
    'bottom-up))
(newline)


(pretty-write
  (closure-conversion
    '(lambda (v)
       (app
         (lambda (x)
           (+ x v)) u))
    'bottom-up))
(newline)


(pretty-write
  (closure-conversion
    '(lambda ()
       (app
         (lambda (x)
           (+ x v)) u))
    'bottom-up))
(newline)


(pretty-write
  (closure-conversion
    '(let ([f (lambda (v)
                (+ v u))])
       (app f 3))
    'bottom-up))
(newline)

