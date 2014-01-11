(module Env racket
  (require "../base/utils.rkt"
           "env.rkt"
           "../types/type.rkt"
           "../types/store.rkt")
  (provide apply
           extend
           extends*
           (rename-out [CEnv create]
                       [CEnv? is-env?])
           )
  (struct CEnv (v-env t-env))
  (define (apply env v)
    (match v
      [(V-Var v)
       (match env
         [(CEnv v-env _)
          (deref (apply-env v-env v))])]
      [(T-Var t)
       (match env
         [(CEnv _ t-env)
          (deref (apply-env t-env t))])]
      [_ (error 'Env.apply "variable:~a is not matched" v)]
      ))
  (define (extend var val env)
    (match var
      [(V-Var v)
       (match env
         [(CEnv v-env t-env)
          (CEnv (extend-env v (newref val) v-env)
                t-env)])]
      [(T-Var t)
       (match env
         [(CEnv v-env t-env)
          (CEnv v-env
                (extend-env t (newref val) t-env))])]))
  ; let* scope
  (define (extends* vars vals env)
    (foldl
      (lambda (var val env)
        (extend var val env))
      env
      vars
      vals))
  ; ** scope. For letrec
  (define (extends** vars vals env)
    (match vars
      [(? list-of V-Var?)
       (match env
         [(CEnv v-env t-env)
          (CEnv
            (extend-envs (map V-Var-v vars)
                         (newrefs vals)
                         v-env)
            t-env)])]
      [(? list-of T-Var?)
       (match env
         [(CEnv v-env t-env)
          (CEnv
            v-env
            (extend-envs (map T-Var-v vars)
                         (newrefs vals)
                         t-env))])]))

  (module* test racket
    (require (prefix-in Env: (submod "..")))
    (require "env.rkt"
             "../base/utils.rkt"
             "../types/type.rkt"
             "../types/store.rkt")
    (initialize-store!)
    (let ((env (Env:create (empty-env) (empty-env))))
      (check eq? (Env:is-env? env) #t)
      (let ((env (Env:extend (V-Var 'v)
                             'int
                             env)))
        (check eq? (Env:apply env (V-Var 'v)) 'int)
        (let ((env (Env:extend (T-Var 't)
                               'int
                               env)))
          (check eq? (Env:apply env (T-Var 't)) 'int)
        )))
    )
  )
