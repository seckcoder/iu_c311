#lang eopl

(require "../base/utils.rkt")
(require "store.rkt")
(require "grammar.rkt")
(require "ds.rkt")
(require "thread.rkt")

(require data/queue)

(provide interp)

(define ready-queue (make-queue))

(define apply-proc
  (lambda (proc1 arg cont)
    (cases
      proc proc1
      (closure
        (var body env)
        (let* ((ref (newref arg))
               (new-env (extend-env var
                                    ref
                                    env)))
          (interp-exp/k body new-env cont)))
      (else (eopl:error 'apply-proc "invalid procedure value:" proc1)))))

(define apply-cont
  (lambda (cont exp-val)
    (cases continuation cont
      (end-cont
        ()
        (set-thread-result! (current-thread) exp-val)
        (if (queue-empty? ready-queue)
          ; all actions finished
          exp-val
          ; start the next thread, current thread is dropped
          (begin
            (set-current-thread! (dequeue! ready-queue))
            (thread-step (current-thread)))))
      (zero1-cont
        (saved-cont)
        (apply-cont saved-cont (boolval (zero? (expval->numval exp-val)))))
      (let-exp-cont
        (var env body saved-cont)
        (let* ((ref (newref exp-val))
               (new-env (extend-env var
                                   ref
                                   env)))
          (interp-exp/k body new-env saved-cont)))
      (diff1-cont
        (subtractor-exp env saved-cont)
        (interp-exp/k subtractor-exp env
                      (diff2-cont exp-val env saved-cont)))
      (diff2-cont
        (minuend-val env saved-cont)
        (apply-cont saved-cont (numval (- (expval->numval minuend-val)
                                         (expval->numval exp-val)))))
      (if-test-cont
        (sbj-exp else-exp env saved-cont)
        (if (expval->boolval exp-val)
          (interp-exp/k sbj-exp env saved-cont)
          (interp-exp/k else-exp env saved-cont)))
      (rator-cont
        (arg-exp env saved-cont)
        (interp-exp/k arg-exp env (rand-cont (expval->procval exp-val) env saved-cont)))
      (rand-cont
        (proc env saved-cont)
        (apply-proc proc exp-val saved-cont))
      (cons1-cont
        (cdrv-exp env saved-cont)
        (interp-exp/k cdrv-exp env (cons2-cont exp-val env saved-cont)))
      (cons2-cont
        (carv env saved-cont)
        (apply-cont saved-cont (listval (cons carv
                                             (expval->listval exp-val)))))
      (car-exp-cont
        (saved-cont)
        (apply-cont saved-cont (car (expval->listval exp-val))))
      (cdr-exp-cont
        (saved-cont)
        (apply-cont saved-cont (listval (cdr (expval->listval exp-val)))))
      (is-empty-exp-cont
        (saved-cont)
        (apply-cont saved-cont (boolval (null? (expval->listval exp-val)))))
      (multi-exp-cont
        (exps accum-op accum env saved-cont)
        (interp-exps/k exps
                       accum-op
                       (accum-op exp-val accum)
                       env
                       saved-cont))
      (set-rhs-cont
        (var-ref env saved-cont)
        (apply-cont saved-cont (setref! var-ref exp-val)))
      (mult-cont1
        (exp2 env saved-cont)
        (interp-exp/k exp2 env (mult-cont2 exp-val saved-cont)))
      (mult-cont2
        (val1 saved-cont)
        (apply-cont saved-cont (numval (* (expval->numval val1)
                                         (expval->numval exp-val)))))
      (print-cont
        (saved-cont)
        (display (expval->normalval exp-val))(newline)
        (apply-cont saved-cont exp-val))
      (spawn-cont
        (env saved-cont)
        (cases proc (expval->procval exp-val)
          (closure
            (var body env)
            ; any solution to fix this?
            ; duplicate code. ugly
            (apply-cont saved-cont (threadval (spawn-thread! body env (end-cont)))))))
      )))

(define interp-exps/k
  (lambda (exps accum-op accum env cont)
    (if (null? exps)
      (apply-cont cont accum)
      (interp-exp/k (car exps)
                    env
                    (multi-exp-cont (cdr exps)
                                    accum-op
                                    accum
                                    env
                                    cont)))))

; the start of evil...
(define interp-exp/k
  (lambda (exp env cont)
    (let ((cur-thd (current-thread)))
      (cond ((thread-expired? cur-thd)
             (thread-reset-time! cur-thd)
             (set-thread-exp! cur-thd exp)
             (set-thread-env! cur-thd env)
             (set-thread-cont! cur-thd cont)
             (enqueue! ready-queue cur-thd)
             (set-current-thread! (dequeue! ready-queue))
             (thread-step (current-thread)))
            (else
              (thread-inc-time! cur-thd)
              (cases
                expression exp
                (const-exp
                  (num)
                  (apply-cont cont (numval num)))
                (diff-exp
                  (minuend subtractor)
                  (interp-exp/k minuend env (diff1-cont subtractor env cont)))
                (zero?-exp
                  (exp)
                  (interp-exp/k exp env (zero1-cont cont)))
                (if-exp
                  (predicate sbj-exp else-exp)
                  (interp-exp/k predicate env (if-test-cont sbj-exp else-exp env cont)))
                (var-exp
                  (var)
                  (apply-cont cont (deref (apply-env env var))))
                (let-exp
                  (var exp1 body)
                  (interp-exp/k exp1 env (let-exp-cont var env body cont)))
                (proc-exp
                  (var body)
                  (apply-cont cont (procval (closure var body env))))
                (call-exp
                  (exp1 exp2)
                  (interp-exp/k exp1 env (rator-cont exp2 env cont)))
                (letrec-exp
                  (p-name b-var b-body letrec-body)
                  (let ((new-env (extend-env-recursively p-name
                                                         b-var
                                                         b-body
                                                         env)))
                    (interp-exp/k letrec-body new-env cont)))
                (cons-exp
                  (carv-exp cdrv-exp)
                  (interp-exp/k carv-exp env (cons1-cont cdrv-exp env cont)))
                (car-exp
                  (lst-exp)
                  (interp-exp/k lst-exp env (car-exp-cont cont)))
                (cdr-exp
                  (lst-exp)
                  (interp-exp/k lst-exp env (cdr-exp-cont cont)))
                (empty-lst-exp
                  ()
                  (apply-cont cont (listval '())))
                (is-empty-exp
                  (lst-exp)
                  (interp-exp/k lst-exp env (is-empty-exp-cont cont)))
                (list-exp
                  (exps)
                  (interp-exps/k exps
                                 (lambda (val accum)
                                   (listval (append (expval->listval accum)
                                                    (list val))))
                                 (listval '())
                                 env
                                 cont))
                (compound-exp
                  (exps)
                  (interp-exps/k exps
                                 (lambda (val accum)
                                   val)
                                 (listval '())
                                 env
                                 cont))
                (set-exp
                  (var exp)
                  (interp-exp/k exp
                                env
                                (set-rhs-cont (apply-env env var) env cont)))
                (mult-exp
                  (exp1 exp2)
                  (interp-exp/k exp1 env (mult-cont1 exp2 env cont)))
                (print-exp
                  (exp)
                  (interp-exp/k exp env (print-cont cont)))
                (spawn-exp
                  (exp)
                  (interp-exp/k exp env (spawn-cont env cont)))
                ))))))


(define spawn-thread!
  (lambda (exp env cont)
    (let ((thd (make-new-thread exp env cont)))
      (enqueue! ready-queue thd)
      thd)))

(define thread-step
  (lambda (thd)
    (let ((exp (thread-exp thd))
          (env (thread-env thd))
          (cont (thread-cont thd)))
      (interp-exp/k exp env cont))))

(define run
  (lambda ()
    ; currently, we simply wait for all threads to finish.
    (thread-step (current-thread))))

(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (initialize-store!)
        (let ((main-thread (spawn-thread! exp (empty-env) (end-cont))))
          (set-current-thread! main-thread)
          (run)
          (expval->normalval (thread-result main-thread)))))))
