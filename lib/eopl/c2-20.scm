(library
  (eopl c2-20)
  (export eopl-c2-20)
  (import (rnrs)
          (elegant-weapons tester)
          )

  ; data type : bi-bintree(bi-directional binary tree)
  ; bi-bintree := (bintree parent)

  (define number->bi-bintree
    (lambda (n)
      (make-bi-bintree (make-bintree n '() '())
                       '())))
  (define make-bi-bintree
    (lambda (bintree parent)
      (list bintree parent)))
  (define bi-bintree->bintree car)
  (define bi-bintree->parent cadr)
  (define at-leaf?
    (lambda (bi-bt)
      (null? (bi-bintree->bintree bi-bt))))
  (define at-root?
    (lambda (bi-bt)
      (null? (bi-bintree->parent bi-bt))))
  (define at-left?
    (lambda (bi-bt)
      (eq? (parent->left (bi-bintree->parent bi-bt)))))
  (define at-right?
    (lambda (bi-bt)
      (eq? (parent->right (bi-bintree->parent bi-bt)))))
  (define bi-bintree->root
    (lambda (bi-bt)
      (bintree->root (bi-bintree->bintree bi-bt))))

  ; data type : bintree(binary tree)
  ; bintree := '() | (Int bintree bintree)
  (define make-bintree
    (lambda (root left right)
      (list root left right)))
  (define bintree->root car)
  (define bintree->left cadr)
  (define bintree->right caddr)

  ; data type : parent of binary tree
  ; parent := '() | (Int 'left bintree parent) | (Int bintree 'right parent)
  (define make-parent
    (lambda (root left right parent)
      (list root left right parent)))
  (define parent->root car)
  (define parent->left cadr)
  (define parent->right caddr)
  (define parent->parent cadddr)

  ; move bi-bintree up
  ; bi-bintree := (bintree parent)
  ; ->
  ; (bintree-of-parent parent-s-parent)
  (define move-up
    (lambda (bi-bt)
      (cond ((at-root? bi-bt)
             (error 'move-up "cannot move root up"))
            ((at-left? bi-bt)
             (let ((parent (bi-bintree->parent bi-bt)))
               (make-bi-bintree (make-bintree (parent->root parent)
                                              (bi-bintree->bintree bi-bt)
                                              (parent->right parent))
                                (parent->parent parent))))
            ((at-right? bi-bt)
             (let ((parent (bi-bintree->parent bi-bt)))
               (make-bi-bintree (make-bintree (parent->root parent)
                                              (parent->left parent)
                                              (bi-bintree->bintree bi-bt))
                                (parent->parent parent))))
            (else
              (error 'move-up "something wrong")))))

  ; move bi-bintree left
  ; bi-bintree := (bintree parent)
  ; (bintree-left add-cur-to-parent)
  (define move-to-left
    (lambda (bi-bt)
      (cond ((at-leaf? bi-bt)
             (error 'move-to-left "at leaf"))
            (else
              (let ((bintree (bi-bintree->bintree bi-bt))
                    (parent (bi-bintree->parent bi-bt)))
                (make-bi-bintree (bintree->left bintree)
                                 (make-parent (bintree->root bintree)
                                              'left
                                              (bintree->right bintree)
                                              parent)))))))

  (define move-to-right
    (lambda (bi-bt)
      (cond ((at-leaf? bi-bt)
             (error 'move-to-right "at leaf"))
            (else
              (let ((bintree (bi-bintree->bintree bi-bt))
                    (parent (bi-bintree->parent bi-bt)))
                (make-bi-bintree (bintree->right bintree)
                                 (make-parent (bintree->root bintree)
                                              (bintree->left bintree)
                                              'right
                                              parent)))))))

  ; insert to the left bintree
  (define insert-to-left
    (lambda (bi-bt elem)
      (let ((bintree (bi-bintree->bintree bi-bt)))
        (make-bi-bintree (make-bintree (bintree->root bintree)
                                       (make-bintree elem
                                                     (bintree->left bintree)
                                                     '())
                                       (bintree->right bintree))
                         (bi-bintree->parent bi-bt)))))

  (define insert-to-right
    (lambda (bi-bt elem)
      (let ((bintree (bi-bintree->bintree bi-bt)))
        (make-bi-bintree (make-bintree (bintree->root bintree)
                                       (bintree->left bintree)
                                       (make-bintree elem
                                                     '()
                                                     (bintree->right bintree)))
                         (bi-bintree->parent bi-bt)))))

  (define-test-suite
    eopl-c2-20
    (bi-directional-binary-tree
      (lambda (fail)
        (let ((bi-bintree (number->bi-bintree 10)))
          (let ((bi-bintree (insert-to-right (insert-to-left bi-bintree 7)
                                             8)))
            (or (equal? (at-root? bi-bintree) #t) (fail))
            (or (equal? (bi-bintree->root bi-bintree) 10) (fail))
            (let ((bi-bintree (move-to-left bi-bintree)))
              (or (equal? (bi-bintree->root bi-bintree) 7) (fail))
              (let ((bi-bintree (insert-to-left bi-bintree 5)))
                (let ((bi-bintree (move-to-left bi-bintree)))
                  (let ((bi-bintree (move-up (move-up bi-bintree))))
                    (or (equal? (bi-bintree->root bi-bintree) 10) (fail))
                    (or (equal? (at-root? bi-bintree) #t) (fail))))))
            (let ((bi-bintree (move-to-right bi-bintree)))
              (or (equal? (bi-bintree->root bi-bintree) 8) (fail)))
            )))))
  )
