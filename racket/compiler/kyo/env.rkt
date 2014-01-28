#lang racket

(provide h:ext
         h:exts
         h:apply
         initial-tenv
         initial-venv)

(module tbl racket
  (provide (all-defined-out))
  (define (ext h k v)
    (hash-set h k v))
  (define (exts h ks vs)
    (foldl
      (lambda (k v h)
        (ext k h v))
      h
      ks
      vs))
  (define (apply h k)
    (hash-ref h k (lambda ()
                    (error 'apply "cannot find ~a" k))))
  (define (empty)
    (make-immutable-hasheq))
  )

(require (prefix-in h: 'tbl))

(define (initial-tenv)
  (h:empty))

(define (initial-venv)
  (h:empty))
