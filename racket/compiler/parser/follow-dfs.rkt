        ; build the follow-table according to the acyclic graph
        (define (dfs)
          (let ((v* (g:vs graph)))
            (foldl
              (lambda (v acc)
                (match acc
                  [(list visited follow-table)
                   (dfs1 visited v follow-table (set))]))
              (list (empty-visited) follow-table)
              v*)))
        (define (dfs1 visited v follow-table folo-set-acc)
          (if (set-member? visited v)
            (list visited follow-table)
            (match (g:bdl-by-v v)
              [(g:bundle v wset vbset)
               (let* ((ws (set->list wset))
                      (u-folo-set (union-follow-sets follow-table (set->list vset)))
                      (follow-table (set-follow-sets follow-table u-folo-set vset)))
                 (foldl
                   (lambda (w acc)
                     (match acc
                       [(list visited follow-table)
                         (dfs1 (set-add visited v)
                               w
                               follow-table
                               (set-union folo-set-acc u-folo-set))]))
                   (list visited follow-table)
                   ws))])))

