#lang eopl

(require racket/match)

(provide make-exception-handler
         exception-handler-push!
         exception-handler-pop!
         initialize-exception-handlers!
         lookup-exception-handler!)

(define exception-handlers '())

; default exceptions: 1 - 100
  ; 1: no exp in compound exp
  ; 2: divide by zero
; user exceptions: 101 - ...

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

(define handle-internal-exception
  (lambda (id)
    (cond ((= id 1)
           (eopl:error 'internal-exception "No expression in compound exception"))
          ((= id 2)
           (eopl:error 'internal-exception "divide by zero"))
          ((<= id 100)
            (eopl:error 'internal-exception "Unknown interal exception:~s" id))
          (else
            #f))))

(define lookup-exception-handler!
  (lambda (searched-id)
    (cond ((no-exception?)
           (if (not (handle-internal-exception searched-id))
             (eopl:error 'exception "Cannot handle exception:~s" searched-id)
             'handled-as-internal-exception))
          (else
            (let ((handler (exception-handler-pop!)))
              (match handler
                [(list id things* ...)
                 (if (eq? id searched-id)
                   handler
                   (lookup-exception-handler! searched-id))]))))))
