#lang eopl

(require "grammar.rkt")
(require "store.rkt")
(require "../base/queue.rkt")
(require "../base/utils.rkt")

(provide (all-defined-out))
; expval := Int | Bool | Proc
; during the implemention, I find this datatype actually
; useless...
(define-datatype
  expval expval?
  (numval
    (int integer?))
  (boolval
    (bool boolean?))
  (listval
    (list list?))
  (procval
    (proc proc?))
  (threadval
    (int integer?))
  (mutexval
    (mutex mutex?))
  )

(define-datatype
  proc proc?
  (closure
    (var symbol?)
    (body expression?) 
    (env environment?)))

(define thread? procedure?)
(define the-initial-thread-id 'uninitialized)
(define set-initial-thread-id!
  (lambda (id)
    (set! the-initial-thread-id id)))
(define the-current-thread-id 'uninitialized)
(define current-thread-id
  (lambda ()
    the-current-thread-id))
(define set-current-thread-id!
  (lambda (id)
    ;(println "update current thread id as:~s" id)
    (set! the-current-thread-id id)))

(define gen-thread-id
  (lambda ()
    (set-initial-thread-id! (add1 the-initial-thread-id))
    the-initial-thread-id))

(define make-new-thread
  (lambda (proc)
    (let ((tid (gen-thread-id)))
      ;(println "create thread:~s in thread:~s" tid (current-thread-id))
      (list tid proc))))

(define thread-start
  (lambda (thd)
    ;(println "start thread:~s" (thread-id thd))
    (set-current-thread-id! (thread-id thd))
    ((cadr thd))))

; used when reput the thread into ready queue
(define remake-thread
  (lambda (proc)
    ;(println "remake thread:~s" (current-thread-id))
    (list (current-thread-id) proc)))

(define thread-id
  (lambda (thd)
    (car thd)))


(define expval->normalval
  (lambda (val)
    (cases expval val
      (numval
        (num)
        num)
      (boolval
        (bool)
        bool)
      (listval
        (lst)
        (map expval->normalval lst))
      (procval
        (proc)
        proc)
      (threadval
        (thdId)
        thdId)
      (mutexval
        (mtx)
        mtx)
      )))

(define expval->listval
  (lambda (val)
    (cases expval val
      (listval
        (lst)
        lst)
      (else
        (eopl:error 'expval->listval "~s is not a list" val)))))

(define expval->numval
  (lambda (val)
    (cases expval val
      (numval
        (num)
        num)
      (else
        (eopl:error 'expval->numval "~s is not a num" val)))))

(define expval->boolval
  (lambda (val)
    (cases expval val
      (boolval
        (bool)
        bool)
      (else
        (eopl:error 'expval->boolval "~s is not a bool" val)))))
(define expval->procval
  (lambda (val)
    (cases expval val
      (procval
        (proc)
        proc)
      (else
        (eopl:error 'expval->procval "~s is not a proc" val)))))

(define expval->mutexval
  (lambda (val)
    (cases expval val
      (mutexval
        (mtx)
        mtx)
      (else
        (eopl:error 'expval->mutexval "~s is not a mutex" val)))))
; environment

; env := '() | (var val env)
(define-datatype
  environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (ref reference?)
    (env environment?)))


(define print-env
  (lambda (env)
    (cases environment env
      (empty-env
        ()
        'ok)
      (extend-env
        (var ref env)
        (display var)
        (display ":")
        (print-exp-val (deref ref))
        (display " ")
        (print-env env)))))

(define print-exp-val
  (lambda (val)
    (cases expval val
      (numval
        (num)
        (display num))
      (boolval
        (bool)
        (display bool))
      (listval
        (lst)
        (display "[")
        (map print-exp-val lst)
        (display "]")
        )
      (procval
        (proc)
        (display "a proc"))
      (threadval
        (thdId)
        (display "thread:")
        (display thdId))
      (mutexval
        (mtx)
        (display "a mutex"))
      )))


(define extend-env-recursively
  (lambda (p-name b-var b-body env)
    (let* ((proc-ref (newref '()))
           (new-env (extend-env p-name
                                proc-ref
                                env)))
      (setref! proc-ref (procval (closure b-var
                                          b-body
                                          new-env)))
      new-env)))

(define apply-env
  (lambda (env search-var)
    (cases
      environment env
      (empty-env
        ()
        (eopl:error 'apply-env "var:~s not found" search-var))
      (extend-env
        (var val inherited-env)
        (if (eq? var search-var)
          val
          (apply-env inherited-env search-var)))
      )))

(define-datatype mutex mutex?
  (a-mutex
    (ref-to-close reference?)
    (ref-to-wait-queue reference?)))

(define the-mutexes '())
(define get-mutexes
  (lambda ()
    the-mutexes))
(define set-mutexes!
  (lambda (v)
    (set! the-mutexes v)))

(define make-new-mutex
  (lambda ()
    (let ((mtx (a-mutex
                 (newref #f)
                 (newref (empty-fq)))))
      (set-mutexes! (cons mtx
                          the-mutexes))
      mtx)))

(define closed-mutex?
  (lambda (mtx)
    (cases mutex mtx
      (a-mutex
        (ref-to-close _)
        (deref ref-to-close)))))

(define en-mutex-wait-queue!
  (lambda (mtx thd)
    ;(println "put thread ~s to mutex wait queue" (thread-id thd))
    (cases mutex mtx
      (a-mutex
        (_ ref-to-wait-queue)
        (setref! ref-to-wait-queue
                 (enfq (deref ref-to-wait-queue)
                       thd))))))

(define de-mutex-wait-queue!
  (lambda (mtx callback)
    (cases mutex mtx
      (a-mutex
        (_ ref-to-wait-queue)
        (defq (deref ref-to-wait-queue)
              (lambda (front fq)
                (setref! ref-to-wait-queue fq)
                (callback front fq)))))))

(define close-mutex!
  (lambda (mtx)
    (cases mutex mtx
      (a-mutex
        (ref-to-close _)
        (setref! ref-to-close #t)))))

(define open-mutex!
  (lambda (mtx)
    (cases mutex mtx
      (a-mutex
        (ref-to-close _)
        (setref! ref-to-close #f)))))

(define mutex-wait-queue-empty?
  (lambda (mtx)
    (cases mutex mtx
      (a-mutex
        (_ ref-to-wait-queue)
        (fq-empty? (deref ref-to-wait-queue))))))

(define mutex-wait-queue-filter!
  (lambda (mtx pred)
    (cases mutex mtx
      (a-mutex
        (_ ref-to-wait-queue)
        (let ((fq (deref ref-to-wait-queue)))
          (setref! ref-to-wait-queue (fq-filter fq pred))
          )))))

(define mutex-wait-queue-ref
  (lambda (mtx)
    (cases mutex mtx
      (a-mutex
        (_ ref-to-wait-queue)
        ref-to-wait-queue))))
