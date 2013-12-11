#lang racket
(require "interp.rkt"
         "../../parser.rkt")


(define (example1)
  ; Q: what's the value of cc?
  ; A: cc is the continuation of the `then` expression in `if`,
  ;    it also equals the continuation of `if` expression,
  ;    therefore it's the continuation after if
  (meval '(let ((start #f))
            (if (not start)
              (call/cc (lambda (cc)
                         (set! start cc))))
            (display "Going to invoke (start)\n")
            (start)))
  )

(define (example2)
  ; Q: what's the value of cc?
  ; A: cc = k because in call/cc the argument pass to `k`
  ;    is regarded as the result of evaluating call/cc
  (meval '((lambda (cc)
             cc)
           (call/cc (lambda (k)
                      (k k)))))
  )

(example2)
