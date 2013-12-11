#lang racket
(require racket/pretty
         "interp.rkt"
         "../../parser.rkt"
         "../../base/utils.rkt")


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

(define (example3)
  ; Q: how the code runs?
  ; A: cc send the argument as the result of evaluating call/cc,
  ;    therefore, after call: `(cc 1) 1 is returned as the result of call/cc,
  ;    ie, the result of current-continuation
  (define (current-continuation)
    (call/cc (lambda (cc) (cc cc))))

  (let ((cc (current-continuation)))
    (printf "~v\n" cc)
    (cond ((continuation? cc)
           (cc 1))
          ((= cc 1) (println "bingo"))
          (else (println "contract violation")))
    ))

(define (example4)
  (meval
    '(let ((current-continuation (lambda ()
                                   (call/cc (lambda (cc)
                                              (cc cc))))))
       (let ((cc (current-continuation)))
         (printf "~v\n" cc)
         (cond ((number? cc) (print "bingo"))
               (else
                 (cc 1))))
       )
    ))

(define (example5)
  ; go-when
  (let ((right-now (lambda ()
                     (call/cc (lambda (cc)
                                (cc cc)))))
        (go-when (lambda (then)
                   (then then))))
    ; Q: How the code runs?
    ; A: It implements an infinite loop. right-now returns the current continuation,
    ;    go-when makes the continuation returned as the result of continuation.
    ;    So when go-when called, it returns to the point of assigning result of `(right-now)
    ;    to `moment`. At the same time, moment is always continuation since it passes
    ;    continuation as argument(ie, return continuation as result of call/cc)
    (let ((moment (right-now)))
      (display "hello, world")
      (newline)
      (go-when moment))
    ))

