#lang racket

(require "lang.rkt"
         "env.rkt"
         "../../base/utils.rkt")


(define (trans-const v)
  (match v
    [(? number?)
     (t:Int)]
    [(? string?)
     (t:Str)]
    [(? boolean?)
     (t:Bool)]))

(define (trans-ty! tenv t)
  (match t
    [(t:Name n (ref (Some _)))
     t]
    [(t:Name n (ref (None)))
     (let ((new-t (trans-ty! tenv (h:apply tenv n))))
       (match t
         [(t:Name n r)
          ; modify t's inner value
          (set-ref-v! r (Some new-t))
          t]))]
    [_ t]))

(define (type-equal? tenv t1 t2)
  (let loop ((t1 (trans-ty! tenv t1))
             (t2 (trans-ty! tenv t2)))
    (match* (t1 t2)
      [((t:Name n1 (ref (Some t1)))
        (t:Name n2 (ref (Some t2))))
       (loop t1 t2)]
      [((t:Name n1 (ref (Some t1))) t2)
       (loop t1 t2)]
      [(t1 (t:Name n2 (ref (Some t2))))
       (loop t1 t2)]
      [(t1 t2)
       (equal? t1 t2)])))

(define (check-type! exp tenv t0 . ts)
  (let loop ((ts ts))
    (cond
      ((null? ts) (void))
      ((type-equal? tenv t0 (car ts))
       (loop (cdr ts)))
      (else
        (error 'check-type!
               "~a : ~a in ~a not match"
               t0
               (car ts)
               exp)))))

(define (check-int! exp tenv . ts)
  (apply
    check-type!
    `(,exp ,tenv ,(t:Int) ,@ts)))

(define (check-bool! exp tenv . ts)
  (apply
    check-type!
    `(,exp ,tenv ,(t:Bool) ,@ts)))

(define (check-unit! exp tenv . ts)
  (apply
    check-type!
    `(,exp ,tenv ,(t:Unit) ,@ts)))

(define (e:trans! tenv venv)
  (define (tr-seq! exps)
    (cond
      [(null? exps)
       (error 'seq "empty sequence")]
      [(null? (cdr exps))
       (tr! (car exps))]
      [else
        (check-unit! (car exps)
                     tenv
                     (tr! (car exps)))
        (tr-seq! (cdr exps))]))
  (define (tr! exp)
    (match exp
      [(e:const v)
       (trans-const v)]
      [(e:var v)
       (h:apply venv v)]
      [(e:biop op a b)
       (cond
         ((memq op '(+ - * ))
          (check-int! exp tenv (tr! a) (tr! b))
          (t:Int))
         ((memq op '(= < > <= >=))
          (check-int! exp tenv (tr! a) (tr! b))
          (t:Bool))
         ((memq op '(and or))
          (check-bool! exp tenv (tr! a) (tr! b))
          (t:Bool)))]
      [(e:unop op v)
       (cond
         ((memq op '(not))
          (check-bool! exp tenv (tr! v) exp)
          (t:Bool)))]
      [(e:vec t vs)
       (apply
         check-type!
         `(,exp ,tenv ,t ,@(map tr! vs)))
       (t:Vec t)]
      [(e:vecref v i)
       (check-int! i tenv (tr! i))
       (match (tr! v)
         [(t:Vec vt)
          vt])]
      [(e:fun (t:Ft rnd rt) (e:fv v body))
       (match v
         [(Some x)
          ((e:trans! tenv
                     (h:ext venv x rnd))
           body)]
         [(None)
          (tr! body)])
       (t:Ft rnd rt)]
      [(e:seq exps)
       (tr-seq! exps)]
      [(e:set v val)
       (check-type!
         exp
         tenv
         (tr! val)
         (h:apply venv v))
       (t:Unit)]
      [(e:vecset v i val)
       (check-int! i tenv (tr! i))
       (match (tr! v)
         [(t:Vec t)
          (check-type!
            exp
            tenv
            t
            (tr! val))])
       (t:Unit)]
      [(e:ife test then else)
       (check-bool! test tenv (tr! test))
       (let ((then-t (tr! then))
             (else-t (tr! else)))
         (check-type! exp
                      tenv
                      then-t
                      else-t)
         then-t)]
      [(e:lete decls body)
       (match (d:trans*! tenv venv decls)
         [(list tenv venv)
          ((e:trans! tenv venv)
           body)])]
      [(e:app rator rand)
       (match* ((tr! rator) (tr! rand))
         [((t:Ft rnd rt) rand-t)
          (check-type! exp tenv rnd rand-t)
          rt]
         [(_ _) (error 'e:trans! "~a is not a function" rator)]
         )]))
  tr!)

(module+ test-
  (define (test-trans-exp exp)
    ((e:trans! (initial-tenv)
               (initial-venv))
     (parse-exp exp)))
  (test-trans-exp '(+ 1 2))
  (test-trans-exp '(fn ([x : int]) : (vec int)
                     (set! x 3)
                     (vec int x)))
  (test-trans-exp '(let ([x : int 3]
                         [f (fn ([x : int]) : int
                              x)])
                     (if (= x 3)
                       (f x)
                       (f x))))
  )

(define (d:trans! tenv venv decl)
  (match decl
    [(d:tydec id t)
     (list (h:ext tenv id t)
           venv)]
    [(d:vardec id t v)
     (check-type! v
                  t
                  ((e:trans! tenv venv) v))
     (list tenv
           (h:ext venv id t))]
    [(d:fundec id (e:fun t v))
     (let ((venv (h:ext venv id t)))
       (match decl
         [(d:fundec id f)
          ((e:trans! tenv venv) f)])
       (list tenv venv))]
    ))

(define (d:trans*! tenv venv decls)
  (foldl
    (match-lambda*
      [(list decl (list tenv venv))
       (d:trans! tenv venv decl)])
    (list tenv venv)
    decls))

(module+ test
  (define (test-trans-decl . decls)
    (d:trans*! (initial-tenv)
               (initial-venv)
               (map parse-decl decls)))
  #|(test-trans-decl '(type t int)
                   '(type t1 t)
                   '(def v : t1 3))|#
  (test-trans-decl '(type t int)
                   '(def f (fn ([x : t]) : t
                             (if (= x 0)
                               1
                               (+ (f (- x 1))
                                  1)))))
  )

(define (trans! prog)
  (match prog
    [(Program decls exp)
     (match (d:trans*! decls
                       (initial-tenv)
                       (initial-venv))
       [(list tenv venv)
        ((e:trans! tenv venv) exp)])]))
