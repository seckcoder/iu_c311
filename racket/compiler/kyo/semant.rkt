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

(define type-equal? equal?)

(define (check-type! exp t0 . ts)
  (let loop ((ts ts))
    (cond
      ((null? ts) (void))
      ((type-equal? t0 (car ts))
       (loop (cdr ts)))
      (else
        (error 'check-type!
               "~a : ~a in ~a not match"
               t0
               (car ts)
               exp)))))

(define (check-int! exp . ts)
  (apply
    check-type!
    `(,exp ,(t:Int) ,@ts)))

(define (check-bool! exp . ts)
  (apply
    check-type!
    `(,exp ,(t:Bool) ,@ts)))

(define (check-unit! exp . ts)
  (apply
    check-type!
    `(,exp ,(t:Unit) ,@ts)))

(define (e:trans! tenv venv)
  (define (tr-seq! exps)
    (cond
      [(null? exps)
       (error 'seq "empty sequence")]
      [(null? (cdr exps))
       (tr! (car exps))]
      [else
        (check-unit! (car exps)
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
          (check-int! exp (tr! a) (tr! b))
          (t:Int))
         ((memq op '(= < > <= >=))
          (check-int! exp (tr! a) (tr! b))
          (t:Bool))
         ((memq op '(and or))
          (check-bool! exp (tr! a) (tr! b))
          (t:Bool)))]
      [(e:unop op v)
       (cond
         ((memq op '(not))
          (check-bool! (tr! v) exp)
          (t:Bool)))]
      [(e:vec t vs)
       (apply
         check-type!
         `(,exp ,t ,@(map tr! vs)))
       (t:Vec t)]
      [(e:vecref v i)
       (check-int! i (tr! i))
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
         (tr! val)
         (h:apply venv v))
       (t:Unit)]
      [(e:vecset v i val)
       (check-int! i (tr! i))
       (match (tr! v)
         [(t:Vec t)
          (check-type!
            exp
            t
            (tr! val))])
       (t:Unit)]
      [(e:ife test then else)
       (check-bool! test (tr! test))
       (let ((then-t (tr! then))
             (else-t (tr! else)))
         (check-type! exp
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
          (check-type! exp rnd rand-t)
          rt]
         [(_ _) (error 'e:trans! "~a is not a function" rator)]
         )]))
  tr!)

(module+ test
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
     (list tenv
           (h:ext venv id t))]))

(define (d:trans*! tenv venv decls)
  (foldl
    (match-lambda*
      [(list decl (list tenv venv))
       (d:trans! tenv venv decl)])
    (list tenv venv)
    decls))

(define (trans! prog)
  (match prog
    [(Program decls exp)
     (match (d:trans*! decls
                       (initial-tenv)
                       (initial-venv))
       [(list tenv venv)
        ((e:trans! tenv venv) exp)])]))
