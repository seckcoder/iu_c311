(define tree '())
(define cont '())
(define pc '())

(define trampoline!
  (lambda ()
    (while pc
      (pc))))

(define inorder-trav
  (lambda (tr)
    (set! tree tree)
    (set! cont (end-cont))
    (set! pc inorder-trav/k)
    (trampoline!)))

(define inorder-trav/k
  (lambda ()
    (if (node? tree)
      (begin
        (display (data tree))
        (set! pc apply-cont))
      (begin
        (set! tree (left tree))
        (set! cont (trav-left-cont (root tree)
                                   (right tree)
                                   cont))
        (set! pc inorder-trav/k)))))

(define apply-cont
  (lambda ()
    (cases continuation cont
      (trav-left-cont
        (root right saved-cont)
        (display (data root))
        (set! tree right)
        (set! cont saved-cont)
        (set! pc inorder-trav/k))
      (end-cont
        ()
        (set! pc #f)
        'ok))))
