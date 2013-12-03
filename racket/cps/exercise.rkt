; 6.3
; 1

(lambda (x y)
  (q y
     (lambda (val)
       (p (+ 8 x) val))))

; 2
(lambda (x y u v)
  (g x
     y
     (lambda (val)
       (f val
          (+ u v)
          (lambda (val)
            (+ 1 val))))))

; 6.9
; 交换律
; 1 * 1 * 2 * 3 * 4 * 5 = 1 * 5 * 4 * 3 * 2 * 1
