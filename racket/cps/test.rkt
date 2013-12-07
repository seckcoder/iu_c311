#lang eopl

(require "../base/utils.rkt")
(require "tail-form.rkt")
(require "parser/parser-out.rkt")

(define test-prog
  (lambda (prog)
    (interp (parse prog))))

(define eval-prog
  (lambda (prog)
    (display (test-prog prog))))

(define test-prog-eqv 
  (lambda (prog v)
    (check eq? (interp (parse prog)) v)))

(define test-prog-equalv
  (lambda (prog v)
    (check equal? (interp (parse prog)) v)))

(define (test)
  ; test list parse
  (test-prog-equalv ''(1 a b (2 3 (c 5) d 6) (7 8))
                    '(1 a b (2 3 (c 5) d 6) (7 8)))

  ; test if parse
  (test-prog-eqv '(if #t 1) 1)

  ; test if parse
  (test-prog-eqv '(if (zero? 0)
                    (if (number? 0)
                      1
                      2)
                    3)
                 1)

  (test-prog-eqv '(let ((v 1)) v)
                 1)
  (test-prog-eqv '(let ((proc (lambda (v) v) ))
                    (proc 1))
                 1)

  (test-prog-eqv '(letrec ((foo (lambda ()
                                  1)))
                    (foo))
                 1)
  (test-prog-eqv '(letrec ((foo (lambda ()
                                  (bar)))
                           (bar (lambda ()
                                  1)))
                    (foo))
                 1)
  (test-prog-eqv '(letrec ((doublek (lambda (n cont)
                                      (if (zero? n)
                                        (cont 0)
                                        (doublek (- n 1)
                                                 (lambda (val)
                                                   (cont (+ val 2)))))))
                           (double (lambda (n)
                                     (doublek n (lambda (val) val)))))
                    (double 4))
                 8)

  ; 6.13
  (test-prog-equalv '(letrec ((remove* (lambda (n s)
                                         (remove*/k n s (lambda (val)
                                                          val))))
                              (remove*/k (lambda (n s cont)
                                           (if (null? s)
                                             (cont '())
                                             (if (not (list? (car s)))
                                               (if (= n (car s))
                                                 (remove*/k n (cdr s) cont)
                                                 (remove*/k n (cdr s) (lambda (val)
                                                                        (cont (cons (car s) val)))))
                                               (remove*/k n (car s) (lambda (val1)
                                                                      (remove*/k n (cdr s) (lambda (val2)
                                                                                             (cont (cons val1 val2)))))))))))
                       (remove* 10 '(1 2 3 10 (4 3 10))))
                    '(1 2 3 (4 3)))
  )
