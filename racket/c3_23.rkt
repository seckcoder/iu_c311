#lang eopl

(require "proc-lang/interp.rkt")

#|(test-prog "let makerec = proc (f)
                            let d = proc (x)
                                      proc (z) ((f (x x)) z)
                            in proc (n) ((f (d d)) n)
            in let maketimes4 = proc (f)
                                  proc (x)
                                    if zero?(x)
                                    then 0
                                    else -((f -(x,1)), -4)
                in let times4 = (makerec maketimes4)
                   in  (times4 3)")|#

(test-prog "let makemult = proc (maker)
                            proc (x)
                              if zero?(x)
                              then 0
                              else -(((maker maker) -(x,1)), -4)
            in let times4 = proc (x) ((makemult makemult) x)
               in (times4 3)")



; 3.23 in scheme
(display
  (let ((fm (lambda (fact-maker)
              (let ((fact (lambda (x)
                            ((fact-maker fact-maker) x))))
                (lambda (n)
                  (if (zero? n)
                    1
                    (* n
                       (fact
                         (- n 1)))))))))
  ((fm fm) 5)))

; Generalize
(((lambda (f)
    ((lambda (fm)
       (fm fm))
     (lambda (fm)
       (f
         (lambda (x)
           ((fm fm) x))))))
  (lambda (fact)
    (lambda (n)
      (if (zero? n)
        1
        (* n
           (fact
             (- n 1)))))))
 5)
