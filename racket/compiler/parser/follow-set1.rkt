#lang racket

(require "ds.rkt"
         "../../base/utils.rkt"
         "first-set.rkt"
         "constraint.rkt"
         "demos.rkt")

(define (follow-set prods first-table)
  (let ((follow-subst (empty-subst)))
    (define (unify! eq)
      (set! follow-subst (unify follow-subst eq)))
    (define (from-prods prods follow-table)
      (foldl from-prod follow-table prods))
    (define (from-prod prod follow-table)
      (match prod
        [(Production unterm prod-n es)
         (from-es unterm es follow-table)]))

    ; an optimized version of from-es. iter es in 1pass
    ; (in some special cases 2pass)
    (define (from-es unterm es follow-table)
      ; chaoes... Is there any algorithm for this?
      ; (list term|unterm) * (list term) * Hashtable ->
      ; (list Hashtable fset)
      ; es: list of term|unterm to handle
      ; sigma-es: previous unterms. Between sigma-es:e0 is a list of unterm that has sigma in first set.
      ;           This is propagated to add constraint.
      ; follow-table: the follow table
      ; Return as list:
      ;      follow-table: the follow-table
      ;      fset: first(es). first(B1 B2 B3) = first(B1)
      ;                                       U first(B2) when 'sigma in first(B1)
      ;                                       ...
      (define (iter-es es sigma-es follow-table)
        (cond
          ((null? es)
           (for-each
             (lambda (v)
               (unify! (equa-<= (Var unterm)
                                (Var v))))
             sigma-es)
           (list follow-table (set)))
          ((terminal? (car es))
           (match (iter-es (cdr es) '() follow-table)
             [(list follow-table sigma-fset)
              ; sigma-fset is discarded
              (list follow-table (set (car es)))]))
          (else
            (let* ((e0 (car es))
                   (e0-fset (hash-ref first-table e0 (set))))
              (cond ((set-member? e0-fset 'sigma)
                     (match
                       (iter-es (cdr es)
                                (append sigma-es (list e0))
                                follow-table)
                       [(list follow-table sigma-fset)
                        (let ((follow-table
                                (hash-union
                                  follow-table
                                  e0
                                  sigma-fset)))
                          (list follow-table (set-union (set-remove e0-fset 'sigma)
                                                        sigma-fset)))]))
                    (else
                      (match
                        (iter-es (cdr es)
                                 (list e0)
                                 follow-table)
                        [(list follow-table sigma-fset)
                         (let ((follow-table
                                 (hash-union
                                   follow-table
                                   e0
                                   sigma-fset)))
                           (list follow-table sigma-fset))])))))))
      (match (iter-es es '() follow-table)
        [(list follow-table _)
         follow-table]))
    (define (initial-follow-table)
      (hash-set
        (make-immutable-hasheq)
        (Production-unterm (car prods))
        (set '$)))

    (define (cons-nth v lst-of-lst nth)
      (mapi
        (lambda (i l)
          (if (= i nth)
            (cons v l)
            l))
        lst-of-lst))
    (define (group-=s lst-of-eq)
      (foldl
        (lambda (p lst-of-eql)
          (match p
            [(equa v w)
             (match
               (find (lambda (eql)
                       (member v eql))
                     lst-of-eql)
               [(list find-v? v-eql vi)
                (match
                  (find (lambda (eql)
                          (member w eql))
                        lst-of-eql)
                  [(list find-w? w-eql wi)
                   (cond
                     ((and find-v? find-w?
                           (= vi wi))
                      ; ignore this equation 
                      lst-of-eql)
                     ((and find-v? find-w?
                           (not (= vi wi)))
                      ; vi != wi
                      (let ((new-eql (remove-duplicates
                                       (append v-eql w-eql)
                                       equal?)))
                        (cons new-eql
                              (remove-nth
                                (remove-nth lst-of-eql vi)
                                wi))))
                     ((and find-v? (not find-w?))
                      (cons-nth w lst-of-eql vi))
                     ((and (not find-v?) find-w?)
                      (cons-nth v lst-of-eql wi))
                     ((and (not find-v?) (not find-w?))
                      (cons (list v w)
                            lst-of-eql)))])])]))
        '()
        lst-of-eq))
    (define (union-=s follow-table =s)
      (let ((lst-of-eql (group-=s (subst->list =s))))
        (foldl
          (match-lambda*
            [(list (list v* ...) follow-table)
             ; v0 = v1 = v...
             (let ((follow-set
                     (foldl
                       (lambda (v follow-set)
                         (set-union follow-set
                                    (hash-ref follow-table v (set))))
                       (set)
                       v*)))
               (foldl
                 (lambda (v follow-table)
                   (hash-set follow-table v follow-set))
                 follow-table
                 v*))])
          follow-table
          lst-of-eql)))
    (define (union-<=s follow-table <=s)
      (foldl
        (match-lambda**
          [((equa (st-var v)
                  (st-var w))
            follow-table)
           ; v belong w
           (let* ((v-set (hash-ref follow-table v (set)))
                  (w-set (hash-ref follow-table w (set)))
                  (u-vw (set-union v-set w-set)))
             (hash-set follow-table w u-vw))])
        follow-table
        (subst->list <=s)))
    (let ((follow-table (from-prods prods (initial-follow-table))))
      (match follow-subst
        [(subst =s <=s)
         (print-subst '= =s)
         (print-subst '<= <=s)
         (print-table follow-table)
         (printf "\n")
         (union-<=s
           (union-=s follow-table =s)
           <=s)]))
    ))

(module+ test
  (let ((prods (map make-prod arithmetic-prods)))
    (let ((fset (first-set prods)))
      (print-table fset)
      (printf "\n")
      (print-table
        (follow-set prods fset))))
  )
