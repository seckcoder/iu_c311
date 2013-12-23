#lang racket

(provide (all-defined-out))

; monad-do
(define-syntax mdo
  (syntax-rules (<-)
    [(_ bind e) e]
    [(_ bind (v <- e0) e e* ...)
     (bind e0 (lambda (v) (mdo bind e e* ...)))]
    [(_ bind e0 e e* ...)
     (bind e0 (lambda (_) (mdo bind e e*...)))]))
