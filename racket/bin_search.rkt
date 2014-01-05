(define (bin-search vec v cmp)
  (letrec ((bin-search1 (lambda (i j)
                          (cond ((and (= i j)
                                      (eq? (cmp (vector-ref vec i) v)
                                         '=))
                                 i)
                                ((>= i j)
                                 -1)
                                (else
                                  (let* ((mid (quotient (+ i j) 2))
                                         (cmp-res (cmp (vector-ref vec mid) v)))
                                    (case cmp-res
                                      [(>) (bin-search1 i (sub1 mid))]
                                      [(<) (bin-search1 (add1 mid) j)]
                                      [(=) (bin-search1 i mid)]
                                      ))))))
           (bin-search2 (lambda (i j)
                          (cond ((> i j) i)
                                (else
                                  (let* ((mid (quotient (+ i j) 2))
                                         (cmp-res (cmp (vector-ref vec mid) v)))
                                    (case cmp-res
                                      [(<) (bin-search2 (add1 mid) j)]
                                      [(>) (bin-search2 i (sub1 mid))]
                                      [(=) (bin-search2 (add1 mid) j)]
                                      )))))))
    (list (bin-search1 0 (sub1 (vector-length vec)))
          (bin-search2 0 (sub1 (vector-length vec))))))

(bin-search (vector 1 2 2 3 4) 2 (lambda (v1 v2)
                                 (cond
                                   ((> v1 v2) '>)
                                   ((< v1 v2) '<)
                                   ((= v1 v2) '=))))
