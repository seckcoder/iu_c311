(library
  (eopl c2-stack)
  (export eopl-stack
          empty-stack
          empty-stack?
          top
          push
          pop
          )

  (import (rnrs)
          (elegant-weapons tester)
          )

  ; procedural representation
  (define empty-stack
    (lambda ()
      (lambda ()
        '())))

  (define empty-stack?
    (lambda (st)
      (null? (st))))

  (define top
    (lambda (st)
      (car (st))))

  (define pop
    (lambda (st)
      (lambda ()
        (cdr (st)))))

  (define push
    (lambda (st v)
      (lambda ()
        (cons v st))))

  (define-test-suite
    eopl-stack
    (procedural
      (lambda (fail)
        (let ((st (empty-stack)))
          (or (equal? (top (pop (push (push (push st 3) 4) 5)))
                      4)
              (fail))))
      ))
  )
