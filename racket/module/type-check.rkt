#lang racket

(require "../types/type.rkt"
         "parser.rkt" ; for debugging purpose
         "constraint.rkt"
         (prefix-in Env. "type-env.rkt")
         )

(provide type-check-mod
         typeof-t)

(define (typeof-t t env #:expand? [expand? false])
  (match t
    [(or (? simpletype?)
         (? Opaque?)
         (? Var?))
     t]
    [(? T-Var?)
     ; check t is in the env
     (if expand?
       (Env.apply env t)
       (begin
         (Env.apply env t)
         t))]
    [(Pair a d)
     (Pair (typeof-t a env)
           (typeof-t d env))]
    [(Fun vts rt)
     (Fun (map (lambda (v)
                 (typeof-t v env))
               vts)
          (typeof-t rt env))]
    [(Mod vs ts)
     (let loop ((env env)
                (vs-acc '())
                (ts-acc '())
                (vs vs)
                (ts ts))
       (cond ((and (null? vs)
                   (null? ts))
              (Mod vs-acc ts-acc))
             ((or (null? vs)
                  (null? ts))
              (error 'typeof-t "bad type:~a" t))
             (else
               (let ((v (car vs))
                     (t (car ts)))
                 (match v
                   [(? V-Var?)
                    (loop env
                          (append vs-acc (list v))
                          (append ts-acc (list (typeof-t t env)))
                          (cdr vs)
                          (cdr ts)
                          )]
                   [(? T-Var?)
                    (loop (Env.extend v t env)
                          (append vs-acc (list v))
                          (append ts-acc (list (typeof-t t env)))
                          (cdr vs)
                          (cdr ts))]
                   [_ (error 'typeof-t "match module var:~a failed" v)])))))]
    ))

; sig: v -> t
(define (type-check-mod vs1 ts1
                        vs2 ts2
                        env
                        subst
                        exp)
  #|(print-vts vs1 ts1)
  (print-vts vs2 ts2)|#
  (let ((pairs (map list vs2 ts2)))
    (define (iter vs1 ts1
                  env subst)
      (cond
        ((null? vs1) subst)
        (else
          (let ((v1 (car vs1))
                (t1 (typeof-t (car ts1) env #:expand? true)))
            (match (assoc v1 pairs)
              [#f
               (error 'type-check-mod "not find")]
              [(list v2 t2)
               (let ((t2 (typeof-t (car ts2) env #:expand? true)))
                 (match (list t1 t2)
                   [(list (? Opaque?)
                          (? Opaque?))
                    ; Do you know why we have to generate
                    ; another opaque type here? ^_^
                    (iter (cdr vs1) (cdr ts1)
                          (Env.extend v1 t1 env)
                          subst
                          )]
                   [(list (? Opaque?) _)
                    (iter (cdr vs1) (cdr ts1)
                          (Env.extend v1 t2 env)
                          subst
                          )]
                   [(list _ (? Opaque?))
                    (error 'type-check-mod "opaque vs transparent")]
                   [(list _ _)
                    (iter (cdr vs1) (cdr ts1)
                          env
                          ; TODO: some problem here
                          (unify subst t1 t2 exp)
                          )]))])))))
    (iter vs1 ts1 env subst))
   )

(module+ test-
  ; test typeof-t
  (require "../types/store.rkt")
  (define (test-typeof-t t)
    (initialize-store!)
    (typeof-t #:expand? true (parse-t t) (Env.empty)))
  (test-typeof-t '((int) -> int))
  (test-typeof-t '(mod (type t)
                       (type t1 int)
                       (val f ((t1) -> t))))
  )
