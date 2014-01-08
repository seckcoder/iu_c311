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
(struct Unit Type () #:transparent) ; used for (void)
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
; var-types * ret-type
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
                    (error type-name "bad type value:~a ~a" vars types)))
        ; TODO: modify the equal func of Module
        )

; TypeVar
(struct Var Type (v)
        #:transparent
        #:guard (lambda (v tn)
                  (if (typevar? v)
                    v
                    (error tn "bad type value for Var:~a" v))))
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
(define (gen-type-var)
  (Var (typevar)))

#|(define (typeof-simple v)
    (match v
      [(? number? v) (Num)]
      [(? string? v) (Str)]
      [(? boolean? v) (Bool)]
      [(? atom? v) (Atom)]
      [(? null? v) (Nil)]
      [(? pair? v) (Pair (typeof-simple (car v))
                         (typeof-simple (cdr v)))]))|#

(define (pair-no-var? t)
  (match t
    [(Pair a d)
     (and (no-var? a)
          (no-var? d))]))

(define (fun-no-var? tf)
  (match tf
    [(Fun vts rt)
     (andmap no-var? (cons rt vts))]))

(define (mod-no-var? tm)
  (match tm
    [(Mod vars types)
     (andmap no-var? types)]))

; whether the type contain tvar
(define (no-var? t)
  ((allf Num?
         Str?
         Bool?
         Atom?
         Unit?
         Nil?
         (allf Pair? pair-no-var?)
         (allf Fun? fun-no-var?)
         (allf Mod? mod-no-var?)) t))

(define (simpletype? t)
  ((anyf Num?
         Str?
         Bool?
         Atom?
         Unit?
         Nil?) t))

; for type declaration
(define type->sym
  (match-lambda
    [(Num) 'int]
    [(Str) 'str]
    [(Bool) 'bool]
    [(Atom) 'atom]
    [(Unit) 'void]
    [(Nil) '()]
    [(Var v) v]
    [(Pair a d)
     (cons (type->sym a) (type->sym d))]
    [(Fun vts rt)
     `(,(map type->sym vts) -> ,(type->sym rt))]
    [(Mod vars types)
     `(mod ,vars ,(map type->sym types))]
    ))

(define (sym->type t env)
  (match t
    ['int (Num)]
    ['str (Str)]
    ['bool (Bool)]
    ['atom (Atom)]
    ['void (Unit)]
    [(? symbol? v)
     (env v)]
    [(list) (Nil)]
    [(list vts '-> t)
     (Fun (map (lambda (t)
                 (sym->type t env))
               vts)
          (sym->type t env))]
    [`(mod ,vars ,types)
      (Mod vars (map (lambda (t)
                       (sym->type t env))
                     types))]
    [(cons a d)
     (Pair (sym->type a env)
           (sym->type d env))]
    ))

; whether the sexp is of the type format
(define (is-type? s)
  (match-lambda
    [(? simpletype?) #t]
    [(? symbol?)
     ; type declaration type var.
     ; Note this is not `#(Var)`
     #t]
    [(list vts '-> t) #t] ; fun
    [`(mod ,vars ,types) ;mod
      (and ((list-of symbol?) vars)
           (andmap is-type? types))]
    [(cons a d) ; Pair
     (and (is-type? a)
          (is-type? d))]
    [_ #f]
    ))

(define lst-of-t->pair
  (lambda (l)
    (cond ((null? l) (Nil))
          (else
            (Pair (car l)
                  (lst-of-t->pair (cdr l)))))))

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
  (check equal?
         (type->sym
           (lst-of-t->pair (list (Num)
                                 (Num)
                                 (Bool)
                                 (Str))))
         '(int int bool str))
  )
