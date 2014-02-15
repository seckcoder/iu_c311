#lang racket

(provide (all-defined-out))

(define (ext env v val)
  (hash-set env v val))

; ext multi
(define (exts env vs vals)
  (foldl
    (lambda (v val env)
      (ext env v val))
    env
    vs
    vals))

(define (env? v)
  (hash-eq? v))

(define (app env v)
  (hash-ref env v (lambda ()
                    (error 'env:app "~a not found" v))))

(define (empty)
  (make-immutable-hasheq))
