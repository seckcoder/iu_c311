#lang racket

(require "../base/utils.rkt")

(require racket/match)

(provide (all-defined-out))

;types

(struct Type () #:transparent)
(struct Num Type () #:transparent)
(struct Str Type () #:transparent)
(struct Bool Type () #:transparent)
(struct Atom Type () #:transparent)
(struct Nil Type () #:transparent) ; used for empty list
; The intf for guard of struct can be improved
(define type-guard
  (match-lambda*
    [(list v* ... type-name)
     (if ((list-of Type?) v*)
       (apply values v*)
       (error type-name "bad type value:~a" v*))]))
(struct Pair Type (a d)
        #:transparent
        #:guard type-guard)
(struct Fun Type (vts rt)
        #:transparent
        #:guard (lambda (vts rt type-name)
                  (if (andmap Type? (cons rt vts))
                    (values vts rt)
                    (error type-name "bad type value:~a ~a" vts rt))))
(struct Mod Type (vars types)
        #:transparent
        #:guard (lambda (vars types type-name)
                  (if (and ((list-of symbol?) vars)
                           ((list-of Type?) types)
                           (= (length vars)
                              (length types)))
                    (values vars types)
                    (error type-name "bad type value:~a ~a" vars types))))

; TypeVar
(struct Var Type (v))

(define (typeof-simple v)
  (match v
    [(? number? v) (Num)]
    [(? string? v) (Str)]
    [(? boolean? v) (Bool)]
    [(? atom? v) (Atom)]
    [(? null? v) (Nil)]
    [(? list? v) (Pair (typeof-simple (car v))
                       (typeof-simple (cdr v)))]))

; Function and Module are composite/complex type
(define (simple? t)
  (combine Pair? Nil? Atom? Bool? Str? Num?))

; for type declaration
(define type->sym
  (lambda (type)
    (match type
      [(Num) 'int]
      [(Str) 'str]
      [(Bool) 'bool]
      [(Atom) 'atom]
      [(Nil) '()]
      [(Pair a d)
       (cons (type->sym a) (type->sym d))]
      [(Fun vts rt)
       `(,(map type->sym vts) -> ,(type->sym rt))]
      [(Mod vars types)
       `(mod ,vars ,(map type->sym types))]
      )))

(define sym->type
  (lambda (sym)
    (match sym
      ['int (Num)]
      ['str (Str)]
      ['bool (Bool)]
      ['atom (Atom)]
      [(list) (Nil)]
      [(list vts '-> t)
       (Fun (map sym->type vts)
            (sym->type t))]
      [`(mod ,vars ,types)
        (Mod vars (map sym->type types))]
      [(cons a d)
       (Pair (sym->type a)
             (sym->type d))]
      )))

(module+ test
  (define (test-type-sym t)
    (check equal?
           (type->sym (sym->type t))
           t))
  (test-type-sym '((int) -> int))
  (test-type-sym '((int bool) -> int))
  (test-type-sym '())
  (test-type-sym '(mod (v f)
                       (int ((int) -> int))))
  (test-type-sym '(int int . int))
  (test-type-sym '(int int))
  )
