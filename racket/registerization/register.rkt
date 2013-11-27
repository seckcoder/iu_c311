#lang eopl

(require racket/base)

(provide reg-set
         reg-get)

(define register (make-hasheq))

(define reg-set
  (lambda (name val)
    (hash-set! register name val)))

(define reg-set*
  (lambda (name val . rest)
    (reg-set name val)
    (if (not (null? rest))
      (apply reg-set* rest)
      'ok)))

(define reg-get
  (lambda (name)
    (hash-ref register name 'unknown)))
