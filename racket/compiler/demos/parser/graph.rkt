#lang racket

(provide anon-vtx
         wset-by-v
         vs
         cyclic->acyclic
         make-graph
         +bundle
         bundle
         bundle?
         pr-graph
         empty-visited
         visited?
         extend-visited
         bdl-by-v
         )
(define (union g vs)
  (let ((vs-list vs)
        (vs-set (list->set vs)))
    (let-values ([(vs-bdls g)
                  (partition
                    (lambda (bdl)
                      (set-member? vs-set (bundle-v bdl)))
                      g)])
      (let* ((u (gensym))
             (u-wset (set-subtract
                     (foldl
                       (match-lambda*
                         [(list (bundle v wset vbset) u-wset)
                          (set-union u-wset wset)])
                       (set)
                       vs-bdls)
                     vs-set))
             (u-vbset (foldl
                       (match-lambda*
                         [(list (bundle v wset vbset) u-vbset)
                          (set-union u-vbset vbset)])
                       (set)
                       vs-bdls)))
        ; replace
        (+bundle1
          (map
            (match-lambda
              [(bundle v wset vbset)
               (bundle v
                       (let ((int-set (set-intersect wset
                                                     vs-set)))
                         (cond
                           ((set-empty? int-set) wset)
                           (else
                             (set-add (set-subtract wset vs-set)
                                      u))))
                       vbset)])
            g)
          (bundle u u-wset u-vbset))))))

(define (anon-vtx)
  (gensym))
; v->w
(define (wset-by-v g v)
  (bundle-wset (bdl-by-v g v)))

; all vtx of g
(define (vs g)
  (map bundle-v g))
; return an empty graph
(define (empty-graph)
  '())

; v: vertex
; wset: connected points
; vbset: information for vertex
(struct bundle (v wset vbset)
        #:guard (lambda (v wset vbset tname)
                  (if (and (symbol? v)
                           (set? wset )
                           (set? vbset))
                    (values v wset vbset)
                    (error tname "bad value for bundle"))))

; add a bundle to the graph
; if the v:ws is already exist in the graph,
; it will try to extend the bundle
; if the v:ws is not exist in the graph
; it will add a new bundle
; O(bundle-num)
(define (+bundle g bdl)
  (match bdl
    [(bundle v wset vbset)
     (extend-bdl (+vtx g v) bdl)]))
; simply add the bundl to the graph
; assume the vtx is not exist in the graph
(define (+bundle1 g bdl)
  (cons bdl g))

(define (rm-bdl-by-v g v)
  (filter
    (lambda (bdl)
      (not (eq? (bundle-v bdl) v)))
    g))

(define (bdl-by-v g v)
  (match
    (findf
      (lambda (bdl)
        (eq? (bundle-v bdl) v))
      g)
    [#f (error 'bdl-by-v "can't find corresponding bundle of ~a" v)]
    [bdl bdl]))

(define (+vtx g v)
  (match
    (findf
      (lambda (bdl)
        (eq? v (bundle-v bdl)))
      g)
    [#f (+vtx1 g v)]
    [(? bundle? bdl) g]))
(define (+vtx1 g v)
  (cons (bundle v (set) (set))
        g))

; extend bdl of the corresponding bundle in graph
(define (extend-bdl g bdl)
  (match bdl
    [(bundle v wset vbset)
     (map
       (match-lambda
         [(bundle v1 wset1 vbset1)
          (if (eq? v v1)
            (bundle v
                    (set-union wset wset1)
                    (set-union vbset vbset1))
            (bundle v1 wset1 vbset1))])
       g)]))

(define (make-graph edges)
  (let ((g (empty-graph)))
    (foldl
      (match-lambda**
        [((list v (list ws ...)) g)
         (+bundle g
                  (bundle v (list->set ws) (set v)))])
      g
      edges)))

(define (empty-visited)
  (set))

(define (visited? visited v)
  (set-member? visited v))

(define (extend-visited visited v)
  (set-add visited v))

(define (empty-path)
  '())

(define (extend-path path v)
  (cons v path))

(define (in-path? path v)
  (memq v path))

(define (path-take-since pred path)
  (reverse
    (dropf-right path (lambda (v)
                        (not (pred v))))))

(struct cyclic (path visited))
(struct acyclic (visited))

(define (cyclic->acyclic g)
  (define (walk v g path visited-v)
    (cond ((visited? visited-v v)
           (acyclic visited-v))
          (else
            (let ((visited-v (extend-visited visited-v v))
                  (path (extend-path path v)))
              (let ((w* (wset-by-v g v)))
                (let loop ((w* w*)
                           (visited-v visited-v)
                           (path path))
                  (cond
                    ((set-empty? w*) (acyclic visited-v))
                    ((in-path? path (set-first w*))
                     (cyclic
                       (path-take-since (lambda (v)
                                          (equal? v (set-first w*)))
                                        path)
                       visited-v
                       ))
                    (else
                      (match (walk (set-first w*) g path visited-v)
                        [(? cyclic? c) c]
                        [(acyclic visited-v)
                         (loop (set-rest w*)
                               visited-v
                               path)])))))))))
  (define (reformat-graph graph path)
    (union graph path))
  (let loop ((v* (vs g))
             (visited-v (empty-visited)))
    (cond
      ((null? v*) g)
      (else
        (match (walk (car v*) g (empty-path) visited-v)
          [(cyclic path visited-v)
           (cyclic->acyclic (reformat-graph g path))]
          [(acyclic visited-v)
           (loop (cdr v*)
                 visited-v)])))))

(define (pr-graph g)
  (for-each
    (match-lambda
      [(bundle v wset vbset)
       (printf "~a -> ~a | ~a\n" v wset vbset)])
    g))


(module+ test
  (define demo1
    (make-graph
      '((a (b c))
        (b (a))
        (c ())
        (d (e f g))
        (e (f))
        (f ())
        (g ())
        )))
  ;(pr-graph demo1)
  (pr-graph (cyclic->acyclic demo1))
  )
