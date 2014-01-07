#lang racket

(require "../base/utils.rkt")

(require racket/match)

(provide (all-defined-out))

;types

(define (typeof-const v)
  (cond ((number? v) 'int)
        ((string? v) 'str)
        ((boolean? v) 'bool)
        (else #f)))

(define (typeof-sexp v)
  (cond ((atom? v) 'atom)
        ((list? v) 'list)
        (else #f)))

(define (type? t)
  (match t
    [(or 'void 'int 'str 'bool 'atom 'list) #t]
    [`(mod type ,vars ,types)
      (andmap type? types)]
    [(list types t)
     (andmap type? (cons t types))]
    ))

(define (proctype var-type ret-type)
  `(,var-type ,ret-type))

(define (modtype vars types)
  `(mod type ,vars ,types))

(define typevar
  (let ((n -1))
    (lambda ()
      (set! n (+ n 1))
      (string->symbol (string-append "t" (number->string n)))
      )))

(define typevar?
  (lambda (type)
    (and (symbol? type)
         (char=? (string-ref (symbol->string type) 0)
                 #\t))))

(define type->str
  (lambda (type)
    (match type
      [(? symbol? type)
       (symbol->string type)]
      [`(mod type ,vars ,types)
        (format "module: ~a" (map (lambda (var type)
                                    (format "~a : ~s" var (type->str type)))
                                  vars
                                  types))]
      [(list (list (? symbol? type)) ret-type)
       ; tiny optimization, erase bracket for some cases
       (format "~a -> ~a" type ret-type)]
      [(list (list types ...) ret-type)
       (format "~a -> ~a" (map type->str types) (type->str ret-type))]
      )))

(define (simpletype? t)
  (match t
    [(or 'void 'int 'str 'bool 'atom 'list) #t]
    [_ #f]))
