#lang racket

(provide (all-defined-out))

(define (ext env v val)
  (hash-set env v val))

(define (app env v)
  (hash-ref env v (lambda ()
                    (error 'app "~a not found" v))))

(define (empty)
  (make-immutable-hasheq))
