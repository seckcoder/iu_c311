#lang racket

(require "ds.rkt"
         "../../base/utils.rkt"
         "first-set.rkt"
         "demos.rkt"
         (prefix-in g: "graph.rkt")
         )

(provide follow-table)

(define (follow-table prods fst-table)
  (let ((follow-subst '()))
    (define (unify! v w)
      (set! follow-subst (cons (list v w)
                               follow-subst)))
    (define (from-prods prods follow-table)
      (foldl from-prod follow-table prods))
    (define (from-prod prod follow-table)
      (match prod
        [(Production unterm prod-n es)
         (from-es unterm es follow-table)]))
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
               (unify! unterm v))
             sigma-es)
           (list follow-table (set)))
          ((terminal? (car es))
           (match (iter-es (cdr es) '() follow-table)
             [(list follow-table sigma-fset)
              ; sigma-fset is discarded
              (list follow-table (set (car es)))]))
          (else
            (let* ((e0 (car es))
                   (e0-fset (hash-ref fst-table e0 (set))))
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
    (define (make-subset-graph)
      (g:make-graph
        (map
          (match-lambda
            [(list v w)
             (list v (list w))])
          follow-subst)))
    (let ((follow-table (from-prods prods (initial-follow-table))))
      (let ((graph (g:cyclic->acyclic (make-subset-graph))))
        (g:pr-graph graph)
        (define (accumulate-follow-set bdl folo-set-acc follow-table)
          ; folo-set-acc = folo-set-acc U (follow-set of (w:wset))
          ; update the follow-set of v in follow-table
          ; pass folo-set-acc and follow-table...
          (match bdl
            [(g:bundle v wset vbset)
             (let* ((vb* (set->list vbset))
                    (folo-set-acc
                      (foldl
                        (lambda (u folo-set-acc)
                          (set-union folo-set-acc (hash-ref follow-table u (set))))
                        folo-set-acc
                        vb*)))
               (list folo-set-acc
                     (foldl
                       (lambda (u follow-table)
                         (hash-set follow-table u folo-set-acc))
                       follow-table
                       vb*)))]))
        (define (dfs)
          (define dfs1
            (lambda (v ws visited folo-set-acc follow-table)
              (foldl
                (match-lambda*
                  [(list w (list visited follow-table))
                   (if (g:visited? visited w)
                     (list visited follow-table)
                     (let ((w-bdl (g:bdl-by-v graph w)))
                       (match w-bdl
                         [(g:bundle w uset wbset)
                          (match (accumulate-follow-set w-bdl folo-set-acc follow-table)
                            [(list folo-set-acc follow-table)
                             (dfs1
                               w
                               (set->list uset)
                               (g:extend-visited visited w)
                               folo-set-acc
                               follow-table)])])))])
                (list visited follow-table)
                ws)))
          (let ((root (g:anon-vtx)))
            (dfs1 root (g:vs graph)
                  (g:empty-visited) (set)
                  follow-table)))
        (cadr (dfs))
        ))
    ))


(define (test-case)
  (let* ((prods (map make-prod arithmetic-prods))
         (fst-table (first-table prods)))
    (follow-table prods fst-table)))
