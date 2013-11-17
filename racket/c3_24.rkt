#lang eopl

(require "proc-lang/interp.rkt")


(display ((let ((make-odd
                  (lambda (make-odd)
                    (lambda (make-even)
                      (lambda (n)
                        (if (zero? n)
                          #f
                          (((make-even make-odd) make-even)
                           (- n 1))
                          )))))
                (make-even (lambda (make-odd)
                             (lambda (make-even)
                               (lambda (n)
                                 (if (zero? n)
                                   #t
                                   (((make-odd make-odd) make-even)
                                    (- n 1))))))))
            ((make-odd make-odd) make-even))
          3))
