(load "tests-driver.scm")

(load "tests-1.1-req.scm")
(define (compile-program x)
  (emit "   .section	__TEXT,__text,regular,pure_instructions")
  (emit "   .globl	_scheme_entry")
  (emit "   .align	4, 0x90")
  (emit "_scheme_entry:")
  (emit-program x)
  (emit "   ret"))

; 1.2
(load "tests-1.2-req.scm")

(define fxshift 2)
(define fxmask #x03)
(define bool_f #x2F)
(define bool_t #x6F)
(define null_v #x3F)
(define wordsize 4)
(define chartag #x0F)

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))
(define (fixnum? x)
  (and (integer? x)
       (exact? x)
       (<= fxlower x fxupper)))
(define (immediate? x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)))

(define (immediate-rep x)
  (cond
    [(fixnum? x)
     (arithmetic-shift x fxshift)]
    [(boolean? x)
     (if (eq? x #t)
       bool_t
       bool_f)]
    [(char? x)
     (+ (arithmetic-shift (char->integer x) 8)
        chartag)]
    [(and (list? x)
          (null? x))
     null_v]
    ))

(define (emit-program x)
  (emit "   movl $~s, %eax" (immediate-rep x))
  )
