#lang racket

(require "../types/type.rkt"
         "parser.rkt" ; for debugging purpose
         "constraint.rkt"
         (prefix-in Env. "type-env.rkt")
         )

(provide type-check-mod
         )

; sig: v -> t
(define (type-check-mod vs1 ts1
                        vs2 ts2
                        env
                        subst
                        exp)
  (print-vts vs1 ts1)
  (print-vts vs2 ts2)
  (let ((pairs (map list vs2 ts2)))
    (define (iter vs1 ts1
                  env subst
                  v-ts)
      (cond
        ((null? vs1) (reverse v-ts))
        (else
          (let ((v1 (car vs1))
                (t1 (car ts1)))
            (match (assoc v1 pairs)
              [#f
               (error 'type-check-mod "not find")]
              [(list v2 t2)
               (match (list t1 t2)
                 [(list (? Opaque?)
                        (? Opaque?))
                  ; Do you know why we have to generate
                  ; another opaque type here? ^_^
                  (let ((opaque-t (gen-opaque-type)))
                    (iter (cdr vs1) (cdr ts1)
                          (Env.extend v1 opaque-t env)
                          subst
                          (cons (list v1 opaque-t) v-ts)
                          ))]
                 [(list (? Opaque?) _)
                  (iter (cdr vs1) (cdr ts1)
                        (Env.extend v1 t2 env)
                        subst
                        (cons (list v1 (gen-opaque-type)) v-ts)
                        )]
                 [(list _ (? Opaque?))
                  (error 'type-check-mod "opaque vs transparent")]
                 [(list _ _)
                  (iter (cdr vs1) (cdr ts1)
                        env
                        ; TODO: some problem here
                        (unify subst t1 t2 exp)
                        (cons (list v1 t1) v-ts)
                        )])])))))
    (let ((v-ts (iter vs1 ts1
                      env
                      subst
                      '())))
      (list 
        (Mod (map car v-ts)
             (map cadr v-ts))
        subst
        )))
   )
