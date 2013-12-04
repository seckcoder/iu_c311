
(define grammar-al
  '((program
      (tfexp)
      a-program)
    (simple-exp
      (number)
      const-exp)
    (simple-exp
      (identifier)
      var-exp)
    (simple-exp
      ("-(" simple-exp "," simple-exp)
      cps-diff-exp)
    (simple-exp
      ("zero?(" simple-exp ")")
      cps-zero-exp)
    (simple-exp
      ("proc(" (arbno identifier) ")" tfexp)
      cps-proc-exp)
    (tfexp
      (simple-exp)
      simple-exp->exp)
    (tfexp
      ("let" identifier "=" simple-exp in tfexp)
      cps-let-exp)
    (tfexp
      ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tfexp) "in" tfexp)
      cps-letrec-exp)
    (tfexp
      ("if" simple-exp "then" tfexp "else" tfexp)
      cps-if-exp)
    (tfexp
      ("(" simple-exp (arbno simple-exp) ")")
      cps-call-exp)
    ))

; 6.11
(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-exp exp
      (const-exp
        (num)
        (numval num))
      (var-exp
        (var)
        (apply-env env var))
      (cps-diff-exp
        (exp1 exp2)
        (numval (- (expval->numval (value-of-simple-exp exp1))
                   (expval->numval (value-of-simple-exp exp2)))))
      (cps-zero-exp
        (exp)
        (boolval (zero? (expval->numval (value-of-simple-exp exp)))))
      (cps-proc-exp
        (vars body)
        (procval (closure vars body env)))
      )))

(define interp-exp/k
  (lambda ()
    (exp env cont)
    (cases tfexp exp
      (simple-exp->exp
        (simple)
        (apply-cont cont
                    (value-of-simple-exp simple env)))
      (cps-let-exp
        (var val-exp body)
        (let ((val (value-of-simple-exp val-exp env)))
          (let ((new-env (extend-env (list var)
                                     (list val)
                                     env)))
          (interp-exp/k body new-env cont))))
      (cps-letrec-exp
        (p-names b-lst-of-vars b-bodies letrec-body)
        (let ((new-env (extend-rec-env p-names
                                       b-lst-of-vars
                                       b-bodies)))
          (interp-exp/k letrec-body new-env cont)))
      (cps-if-exp
        (pred body-yes body-no)
        (if (value-of-simple-exp pred env)
          (interp-exp/k body-yes env cont)
          (interp-exp/k body-no env cont)))
      (cps-call-exp
        (rator rands)
        (let ((rator-proc (expval->procval (value-of-simple-exp rator)))
              (rand-vals
                (map (lambda (simple)
                       (value-of-simple-exp simple))
                     rands)))
        (apply-proc rator-proc rand-vals cont)))
      )))

(define apply-proc
  (lambda (rator rands cont)
    (cases proc rator
      (closure
        (vars body env)
        (let ((new-env (extend-env vars
                                   rands
                                   env)))
          (interp-exp/k body new-nev cont))))))
