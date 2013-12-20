#lang racket
(require "../lazy.rkt"
         "../base/utils.rkt")

(provide (all-defined-out))

(define var
  (lambda () (vector 'v)))
(define var? vector?)

(define walk
  (lambda (v s)
    (if (not (var? v))
      v
      (let ((ret (assoc v s)))
        (if ret
          (walk (cdr ret) s)
          v)))))

(define walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons (walk* (car v) s)
               (walk* (cdr v) s)))
        (else v)))))

; extend substituion
(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define size-s length)
(define empty-s '())
(define ext-s-check
  (lambda (x v s)
    (cond
      ((occurs? x v s) #f)
      (else (ext-s x v s)))))

(define occurs?
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((eq? x v) #t)
        ((pair? v)
         (or (occurs? x (car v) s)
             (occurs? x (cdr v) s)))
        (else #f)))))


(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v)
         (ext-s v (reify-name (size-s s)) s))
        ((pair? v) (reify-s (cdr v)
                            (reify-s (car v) s)))
        (else s)))))


(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define gen-unify
  (lambda (ext-s)
    (lambda (v w s)
      (let ((v (walk v s))
            (w (walk w s)))
        (cond
          ((eq? v w) s)
          ((var? v)
           (ext-s v w s))
          ((var? w)
           (ext-s w v s))
          ((and (pair? v) (pair? w))
           (cond
             ((unify (car v) (car w) s)
              =>
              (lambda (s)
                (unify (cdr v) (cdr w) s)))
             (else #f)))
          ((equal? v w) s)
          (else #f))))))

(define unify (gen-unify ext-s))
(define unify-check (gen-unify ext-s-check))

(define succeeded?
  (lambda (s)
    (not (null? s))))

(define-syntax run
  (syntax-rules ()
    [(_ n (v) g ...)
     (let ((v (var)))
       (let ((a ((all g ...) empty-s)))
         (s:!!
           (map (lambda (s)
                  (reify (walk* v s)))
                (s:take n a)))))]))

(define unit
  (lambda (s)
    (s:list s)))

(define mzero
  (lambda ()
    '()))

(define ==
  (lambda (v w)
    (lambda (s)
      (cond
        ((unify v w s) => succeed)
        (else (mzero))))))

(define succeed
  (lambda (s)
    (unit s)))

(define fail
  (lambda (s)
    (mzero)))

(define-syntax all
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g] ; just a tiny optimization
    [(_ g^ g ...)
     ; and combination
     (lambda (s)
       (s:flatmap (lambda (s)
                       ((all g ...) s))
                     (g^ s)))]))

(define-syntax anye
  (syntax-rules ()
    [(_ g1 g2)
     ; or combination
     (lambda (s)
       (s:append (g1 s)
                    (g2 s)))]))

(define-syntax conde
  (syntax-rules ()
    [(_) fail]
    [(_ (else g^ g ...))
     (all g^ g ...)]
    [(_ (g^ g ...) c ...)
     (anye (all g^ g ...)
           (conde c ...))]))
