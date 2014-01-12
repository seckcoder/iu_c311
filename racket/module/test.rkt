#lang racket
(require "parser.rkt"
         "../types/type.rkt"
         "../types/store.rkt"
         "type.rkt"
         "type-env.rkt"
         (prefix-in Env. "type-env.rkt")
         "type.rkt")

(module+ test
  ; test parser
  (define (test-parse-t t)
    (check equal?
           (unparse-t (parse-t t))
           t))
  (test-parse-t '((int) -> int))
  (test-parse-t '(mod (type t)
                      (type t1 int)
                      (val f ((t1) -> t))))
  )


(module+ test
  (require rackunit)
  ;(test-typeof '(define v 3))
  #|(test-typeof '(begin
                    (define v 3)
                    (lambda (v)
                      v)))|#
  #|(test-typeof '(module m1
                    (sig
                      (deftype u int)
                      (deftype f ((int) -> int)))
                    (body
                      (define u 3)
                      (define f (lambda (v) v)))))|#
  #|(test-typeof '(module m1
                    (sig
                      (u int))
                    (body
                      (define u 3)
                      (define v 5))))|#

  #|(test-typeof (list
                 '(module m1
                    (sig
                      (type t1)
                      (type t2 int)
                      (val f ((t2) - t1)))
                    (body
                      (type t1 int)
                      (define f (lambda (v) v)))))
               (m1:f 3))|#
  )
