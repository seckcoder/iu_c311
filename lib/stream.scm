(library
  (stream)
  (export cons-stream
          list-stream
          stream-car
          stream-cdr
          stream-null?
          the-empty-stream
          stream-map
          stream-filter
          stream-enumerate-interval
          stream-display
          stream-ref
          stream-add
          stream-minus
          stream-scale
          integers-start-from
          stream-mult
          stream-div
          partial-sum
          integers
          stream-display-n
          stream-negate
          one-zeros
          ones
          interleave
          stream-append
          integral
          integral-delayed
          take
          geometric-series)

  (import (rnrs)
          (elegant-weapons compat))

  (define (memo-proc proc)
    (let ((already-run #f) (result '()))
      (lambda ()
        (if (not already-run)
          (begin (set! result (proc))
                 (set! already-run #t)
                 result)
          result))))

  (define (force delayed-object)
    (delayed-object))


  (define-syntax cons-stream
    (syntax-rules ()
                  ((cons-stream a b)
                   (cons a (memo-proc (delay b))))))

  (define-syntax delay
    (syntax-rules ()
                  ((_ v) (lambda () v))))

  ;(cons a (lambda () b)))))

  (define-syntax list-stream
    (syntax-rules ()
                  [(_) the-empty-stream]
                  [(_ a b ...)
                   (cons-stream a
                                (list-stream b ...))]))

  (define (stream-car stream) (car stream))


  (define (stream-cdr stream) (force (cdr stream)))

  (define the-empty-stream '())

  (define (stream-null? s)
    (null? s))

  ; append s1 to s2, note s1 should not be infinite stream
  (define (stream-append s1 s2)
    (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

  ;  (define (stream-map proc s)
  ;(if (stream-null? s)
  ;the-empty-stream
  ;(cons-stream (proc (stream-car s))
  ;(stream-map proc (stream-cdr s)))))


  ; (stream-map proc stream stream ...)
  (define (stream-map proc . stream-args)
    (if (stream-null? (car stream-args))
      the-empty-stream
      (cons-stream (apply proc (map stream-car stream-args))
                   (apply stream-map (cons proc (map stream-cdr stream-args))))))

  ;(define (stream-filter proc s)
  ;(cond ((stream-null? s) the-empty-stream)
  ;((proc (stream-car s))
  ;(cons-stream (stream-car s)
  ;(stream-filter proc (stream-cdr s))))
  ;(else
  ;(stream-filter proc (stream-cdr s)))))


  (define (stream-filter proc . stream-args)
    (cond ((stream-null? (car stream-args)) the-empty-stream)
          ((apply proc (map stream-car stream-args))
           (cons-stream (map stream-car stream-args)
                        (apply stream-filter (cons proc (map stream-cdr stream-args)))))
          (else
            (apply stream-filter (cons proc (map stream-cdr stream-args))))))

  (define (stream-enumerate-interval low high)
    (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

  (define (stream-for-each proc s)
    (if (stream-null? s)
      the-empty-stream
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

  (define (stream-display s)
    (if (not (stream-null? s))
      (begin
        (display (stream-car s))(display " ")
        (stream-display (stream-cdr s)))))

  (define (stream-display-n s n)
    (define (iter s i)
      (if (< i n)
        (begin
          (display (stream-car s))(display " ")
          (iter (stream-cdr s) (+ i 1)))))
    (iter s 0))

  (define (stream-ref s n)
    (define (iter s-remained i)
      (if (= i n)
        (stream-car s-remained)
        (iter (stream-cdr s-remained) (+ i 1))))
    (iter s 0))

  (define (stream-add s1 s2)
    (stream-map + s1 s2))

  (define (stream-minus s1 s2)
    (stream-map - s1 s2))

  (define (stream-scale s factor)
    (stream-map (lambda (x) (* x factor)) s))

  (define (integers-start-from n)
    (cons-stream n (integers-start-from (+ n 1))))

  (define (stream-mult s1 s2)
    (stream-map * s1 s2))

  (define (stream-div s1 s2)
    (stream-map / s1 s2))

  (define zeros
    (cons-stream 0 zeros))

  (define ones
    (cons-stream 1 ones))

  (define one-zeros (cons-stream 1 zeros))

  (define negative-ones
    (cons-stream -1 negative-ones))

  (define integers
    (cons-stream 1 (stream-add ones integers)))

  ; sum of stream
  (define (partial-sum s)
    (define sum (cons-stream (stream-car s)
                             (stream-add (stream-cdr s)
                                         sum)))
    sum)

  (define (stream-negate s)
    (stream-mult negative-ones
                 s))

  ; interleave two streams
  (define (interleave s1 s2)
    (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2
                               (stream-cdr s1)))))

  (define (integral integrand initial-value dt)
    (define int
      (cons-stream initial-value
                   (stream-add (stream-scale integrand dt)
                               int)))
    int)

  (define (integral-delayed delayed-integrand initial-value dt)
    (define int
      (cons-stream initial-value
                   (let ((integrand (force delayed-integrand)))
                     (stream-add (stream-scale integrand dt)
                                 int))))
    int)

  (define take
    (lambda (n stream)
      (if (= n 0)
        '()
        (cons (stream-car stream)
              (take (sub1 n)
                    (stream-cdr stream))))))

  (define geometric-series
    (lambda (base ratio)
      (cons-stream base
                   (geometric-series (* base ratio)
                                     ratio))))
  )
