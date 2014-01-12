#lang racket

(require "../types/type.rkt")

(provide (all-defined-out))

(define (unify-multi subst types1 types2 exp)
  #|(println types1)(println types2)
  (println (length types1)) (println (length types2))|#
  (foldl (lambda (ty1 ty2 subst)
           (unify subst ty1 ty2 exp))
         subst
         types1
         types2))

(struct equa (left right))

; substitution of a type for a type variable. type[sym = new-type]
(define replace
  (lambda (type tvar new-type)
    ; replace every sym in type with new-type
    (match type
      [(? Var?)
       (if (equal? tvar type)
         new-type
         type)]
      [(Mod vars types)
       (Mod vars (map (lambda (t)
                        (replace t tvar new-type))
                      types))]
      [(Pair a d)
       (Pair (replace a tvar new-type)
             (replace d tvar new-type))]
      [(Fun vts rt)
       (Fun (map (lambda (sub-type)
                   (replace sub-type tvar new-type))
                 vts)
            (replace rt tvar new-type))]
      [_ type]
      )))

; subst[ty1=ty2] replace subst's bindings with ty2
(define (extend-subst subst ty1 ty2)
  (cons (equa ty1 ty2)
        (map (lambda (subst-equa)
               (match subst-equa
                 [(equa tyvar tybind)
                  (equa tyvar (replace tybind ty1 ty2))]))
             subst)))

; occurs check
(define (occurs? tvar type)
  (match type
    [(? Var?) (equal? tvar type)]
    ; check for pair
    [(Pair a d)
     (or (occurs? tvar a)
         (occurs? tvar d))]
    [(Fun vts rt)
     (ormap (lambda (t)
              (occurs? tvar t))
            (cons rt vts))]
    [_ #f]
    ))

; apply subst to type(replace type with bindings)
; or lookup type in subst
(define (apply-subst-to-type subst type)
  (if (no-var? type)
    type  ; a tiny optimization. only apply for type has variable(eopl 7.20)
    (foldl (lambda (subst-equa type)
             (match subst-equa
               [(equa tyvar tybind)
                (replace type tyvar tybind)]))
           type
           subst)))
; unify ty1=ty2 to substitution
(define (unify subst ty1 ty2 exp)
  (let loop ((ty1 (apply-subst-to-type subst ty1))
             (ty2 (apply-subst-to-type subst ty2)))
    (if (equal? ty1 ty2)
      ; useless equation
      subst
      (match ty1
        [(? simpletype?)
         (match ty2
           [(or (? simpletype?)
                (? Fun?)
                (? Mod?)
                (? Pair?))
            (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
           [(? Var?)
            (loop ty2 ty1)])]
        [(? Var?)
         (if (occurs? ty1 ty2)
           (error 'unify "fail occurrence check for expression:~s" exp)
           (extend-subst subst ty1 ty2))]
        [(Fun ty1-vts ty1-rt)
         (match ty2
           [(or (? simpletype?)
                (? Pair?)
                (? Mod?))
            (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
           [(? Var?)
            (loop ty2 ty1)]
           [(Fun ty2-vts ty2-rt)
            (unify-multi subst
                         (cons ty1-rt ty1-vts)
                         (cons ty2-rt ty2-vts)
                         exp)])]
        [(Pair a1 d1)
         (match ty2
           [(or (? simpletype?)
                (? Fun?)
                (? Mod?))
            (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
           [(? Var?)
            (loop ty2 ty1)]
           [(Pair a2 d2)
            (unify-multi subst
                         (list a1 d1)
                         (list a2 d2)
                         exp)]
           )]
        [(? Mod?)
         (match ty2
           [(or (? simpletype?)
                (? Pair?)
                (? Fun?))
            (error 'unify "type error for expression:~s; ~a not equal ~a" exp ty1 ty2)]
           [(? Var?)
            (loop ty2 ty1)]
           [(? Mod?)
            (error "unify module")])]
            ;(unify-mod subst ty1 ty2 exp)])]
        ))))

; TODO: ty1 = ty2
(define (unify-mod subst ty1 ty2 exp)
  (match ty1
    [(Mod vars1 types1)
     (match ty2
       [(Mod vars2 types2)
        (let ((type-pairs (map list vars2 types2)))
          (foldl
            (lambda (var1 type1 subst)
              (match (assq var1 type-pairs)
                [#f
                 (error 'typeof "interface declaration ~a = ~a not exist in module:~s"
                        var1 type1 exp)]
                [(list _ type2)
                 (unify subst
                        type1
                        type2
                        exp)]))
            subst
            vars1
            types1))])]))
