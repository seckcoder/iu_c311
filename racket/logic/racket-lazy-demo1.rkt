#lang racket


(require (prefix-in s: lazy)
         (prefix-in s: "./lazy.rkt"))

(s:! ((s:! s:foo)))

(let ((v (s:flatmap (lambda (v)
                      (list v))
                    s:ones)))
  (s:car (s:! v))
  (s:! (s:cdr (s:! v))))
