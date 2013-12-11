#lang racket
(require "interp.rkt")

(define example1
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
