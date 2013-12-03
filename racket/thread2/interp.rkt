; interpreter of let-lang

#lang eopl

(require "../base/utils.rkt")
(require "store.rkt")
(require "grammar.rkt")
(require "ds.rkt")
(require "scheduler.rkt")
(require racket/file)

(provide (all-defined-out))

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
      (else (eopl:error 'apply-proc "invalid procedure value:~s" proc1)))))


(define-datatype
  continuation continuation?
  (zero1-cont
    (cont continuation?))
  (let-exp-cont
    (var symbol?)
    (env environment?)
    (body expression?)
    (cont continuation?))
  (diff1-cont
    (subtractor-exp expression?)
    (env environment?)
    (cont continuation?))
  (diff2-cont
    ; todo (expval support)
    (minuend-val expval?)
    (env environment?)
    (cont continuation?))
  (if-test-cont
    (sbj-exp expression?)
    (else-exp expression?)
    (env environment?)
    (cont continuation?))
  (rator-cont
    (arg-exp expression?)
    (env environment?)
    (cont continuation?))
  (rand-cont
    (proc proc?)
    (env environment?)
    (cont continuation?))
  (cons1-cont
    (cdrv-exp expression?)
    (env environment?)
    (cont continuation?))
  (cons2-cont
    (carv expval?)
    (env environment?)
    (cont continuation?))
  (car-exp-cont
    (cont continuation?))
  (cdr-exp-cont
    (cont continuation?))
  (is-empty-exp-cont
    (cont continuation?))
  (multi-exp-cont
    (exps (list-of expression?))
    (accum-op procedure?)
    (accum expval?)
    (env environment?)
    (cont continuation?))
  (set-rhs-cont
    (ref reference?)
    (env environment?)
    (cont continuation?))
  (mult-cont1
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (mult-cont2
    (val expval?)
    (cont continuation?))
  (print-cont
    (cont continuation?))
  (spawn-cont
    (cont continuation?))
  (end-subthread-cont)
  (end-mainthread-cont)
  (wait-cont
    (cont continuation?))
  (signal-cont
    (cont continuation?))
  (kill-cont
    (cont continuation?))
  )

(define apply-cont
  (lambda (cont exp-val)
    ;(println "apply-cont:~s" exp-val)
    (if (time-expired?)
      (begin
        ;(println "thread:~s expired" (current-thread-id))
        (place-on-ready-queue!
          (remake-thread
            (lambda ()
              (apply-cont cont exp-val))))
        (run-next-thread))
      (begin
        (decrement-timer!)
        (cases continuation cont
          (zero1-cont
            (next-cont)
            (apply-cont next-cont (boolval (zero? (expval->numval exp-val)))))
          (let-exp-cont
            (var env body next-cont)
            (let* ((ref (newref exp-val))
                   (new-env (extend-env var
                                       ref
                                       env)))
              (interp-exp/k body new-env next-cont)))
          (diff1-cont
            (subtractor-exp env next-cont)
            (interp-exp/k subtractor-exp env
                          (diff2-cont exp-val env next-cont)))
          (diff2-cont
            (minuend-val env next-cont)
            (apply-cont next-cont (numval (- (expval->numval minuend-val)
                                             (expval->numval exp-val)))))
          (if-test-cont
            (sbj-exp else-exp env next-cont)
            (if (expval->boolval exp-val)
              (interp-exp/k sbj-exp env next-cont)
              (interp-exp/k else-exp env next-cont)))
          (rator-cont
            (arg-exp env next-cont)
            (interp-exp/k arg-exp env (rand-cont (expval->procval exp-val) env next-cont)))
          (rand-cont
            (proc env next-cont)
            (apply-proc proc exp-val next-cont))
          (cons1-cont
            (cdrv-exp env next-cont)
            (interp-exp/k cdrv-exp env (cons2-cont exp-val env next-cont)))
          (cons2-cont
            (carv env next-cont)
            (apply-cont next-cont (listval (cons carv
                                                 (expval->listval exp-val)))))
          (car-exp-cont
            (next-cont)
            (apply-cont next-cont (car (expval->listval exp-val))))
          (cdr-exp-cont
            (next-cont)
            (apply-cont next-cont (listval (cdr (expval->listval exp-val)))))
          (is-empty-exp-cont
            (next-cont)
            (apply-cont next-cont (boolval (null? (expval->listval exp-val)))))
          (multi-exp-cont
            (exps accum-op accum env next-cont)
            (interp-exps/k exps
                           accum-op
                           (accum-op exp-val accum)
                           env
                           next-cont))
          (set-rhs-cont
            (var-ref env next-cont)
            (apply-cont next-cont (setref! var-ref exp-val)))
          (mult-cont1
            (exp2 env next-cont)
            (interp-exp/k exp2 env (mult-cont2 exp-val next-cont)))
          (mult-cont2
            (val1 next-cont)
            (apply-cont next-cont (numval (* (expval->numval val1)
                                             (expval->numval exp-val)))))
          (print-cont
            (next-cont)
            (display (expval->normalval exp-val))(newline)
            (apply-cont next-cont exp-val)
            )
          (spawn-cont
            (next-cont)
            (let* ((proc1 (expval->procval exp-val))
                   (thd (make-new-thread (lambda ()
                                           (apply-proc proc1 (numval 28) (end-subthread-cont))))))
              (place-on-ready-queue! thd)
              (apply-cont next-cont (numval (thread-id thd)))))
          (end-subthread-cont
            ()
            ; (println "end of sub thread:~s" (current-thread-id))
            (run-next-thread))
          (end-mainthread-cont
            ()
            (set-final-answer! exp-val)
            (println "end of main thread:~s" (current-thread-id))
            (run-next-thread))
          (wait-cont
            (saved-cont)
            (wait-for-mutex
              (expval->mutexval exp-val)
              (remake-thread
                (lambda ()
                  (apply-cont saved-cont (numval 0))))))
          (signal-cont
            (saved-cont)
            (signal-for-mutex
              (expval->mutexval exp-val)
              (remake-thread
                (lambda ()
                  (apply-cont saved-cont (numval 0))))))
          (kill-cont
            (saved-cont)
            (apply-cont saved-cont (boolval (kill-thread (expval->numval exp-val)))))
          )))))

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

(define interp-exp/k
  (lambda (exp env cont)
    ; (display cont)(newline)(newline)
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
        (interp-exp/k exp env (spawn-cont cont)))
      (mutex-exp
        ()
        (apply-cont cont (mutexval (make-new-mutex))))
      (wait-exp
        (exp)
        (interp-exp/k exp env (wait-cont cont)))
      (signal-exp
        (exp)
        (interp-exp/k exp env (signal-cont cont)))
      (kill-exp
        (exp)
        (interp-exp/k exp env (kill-cont cont)))
      )))

(define interp
  (lambda (datum)
    (cases
      program datum
      (a-program
        (exp)
        (initialize-store!)
        (initialize-scheduler! 5)
        (interp-exp/k exp (empty-env) (end-mainthread-cont))
        (expval->normalval (final-answer))))))
