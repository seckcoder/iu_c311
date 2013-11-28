#lang eopl

(require racket/match)

(provide make-exception-handler
         exception-handler-push!
         exception-handler-pop!
         initialize-exception-handlers!
         lookup-exception-handler!)

(define exception-handlers '())

(define initialize-exception-handlers!
  (lambda ()
    (set! exception-handlers '())))

(define make-exception-handler
  (lambda (excep-id catch-body env cont)
    (list excep-id catch-body env cont)))

(define exception-handler-push!
  (lambda (handler)
    (set! exception-handlers (cons handler exception-handlers))))

(define no-exception?
  (lambda ()
    (null? exception-handlers)))

(define exception-handler-pop!
  (lambda ()
    (let ((handler (car exception-handlers)))
      (set! exception-handlers (cdr exception-handlers))
      handler)))

(define lookup-exception-handler!
  (lambda (searched-id)
    (if (no-exception?)
      (eopl:error 'exception "Cannot handler exception:~s" searched-id)
      (let ((handler (exception-handler-pop!)))
        (match handler
          [(list id things* ...)
           (if (eq? id searched-id)
             handler
             (lookup-exception-handler! searched-id))])))))
