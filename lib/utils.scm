(library
  (utils)

  (export square)

  (import (rnrs))

  (define square
    (lambda (x)
      (* x x)))

  (define-syntax equal??
    (syntax-rules ()
                  ((_ x y)
                   (let ((x^ x) (y^ y))
                     (if (nt (equal? x y))
                       (error 'equal??
                              "~s is not equal to ~s" 'x 'y))))))

  )
