(load "tests-driver.scm")

;(load "tests-1.1-req.scm")
(define (compile-program x)
  (emit "   .section	__TEXT,__text,regular,pure_instructions")
  (emit "   .globl	_scheme_entry")
  (emit "   .align	4, 0x90")
  (emit "_scheme_entry:")
  (emit-program x)
  (emit "   ret"))

; 1.2
;(load "tests-1.2-req.scm")

(define fxshift 2)
(define fxmask #x03)
(define fxtag #x00)
(define bool-f #x2F)
(define bool-t #x6F)
(define null_v #x3F)
(define wordsize 4)
(define charmask #xFF)
(define chartag #x0F)
(define charshift 8)
(define boolshift 6)
(define boolmask #x3F)
(define booltag #x2F)

; Data representation:
; Integer: ....00
; Bool: bool-f | bool-t
; Char: ....00001111

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))
(define (fixnum? x)
  (and (number? x)
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
       bool-t
       bool-f)]
    [(char? x)
     (+ (arithmetic-shift (char->integer x) charshift)
        chartag)]
    [(and (list? x)
          (null? x))
     null_v]
    ))

(define (emit-program x)
  (emit-exp x)
  ;(emit "   movl $~s, %eax" (immediate-rep x))
  )


(require racket/match)
(define (emit-exp exp)
  (match exp
    [(or (? number? x)
         (? boolean? x)
         (? char? x)
         (? null? x))
     (emit "   movl $~s, %eax" (immediate-rep x))]
    [`(add1 ,v)
      (emit-exp v)
      (emit "   addl $~s, %eax" (immediate-rep 1))]
    [`(sub1 ,v)
      (emit-exp v)
      (emit "   subl $~s, %eax" (immediate-rep 1))]
    [`(number->char ,v)
      (emit-exp v)
      ; shift left
      (emit "   shll $~s, %eax" (- charshift fxshift))
      ; change the shifted to char tag
      (emit "   orl $~s, %eax" chartag)]
    [`(char->number ,v)
      (emit-exp v)
      ; just shift right
      (emit "   sarl $~s, %eax" (- charshift fxshift))]
    [`(fixnum? ,v)
      (emit-exp `(number? ,v))]
    [`(number? ,v)
      (emit-exp v)
      (emit "   andb $~s, %al" fxmask)
      (emit "   cmpb $~s, %al" fxtag)
      (emit-eax-to-bool)]
    [`(null? ,v)
      (emit-exp v)
      (emit "   cmpw $~s, %ax" null_v)
      (emit-eax-to-bool)]
    [`(boolean? ,v)
      (emit-exp v)
      (emit "   andb $~s, %al" boolmask)
      (emit "   cmpb $~s, %al" booltag)
      (emit-eax-to-bool)]
    [`(char? ,v)
      (emit-exp v)
      (emit "   andb $~s, %al" charmask)
      (emit "   cmpb $~s, %al" chartag)
      (emit-eax-to-bool)]
    [`(not ,v)
      (emit-exp v)
      (emit "   cmpw $~s, %ax" bool-f)
      ; if equal=#f, we set al to 1, then we transform it to #t.
      ; if equal to other value, we set al to 0, then transformed to #f.
      (emit-eax-to-bool)]
    [`(zero? ,v)
      (emit-exp v)
      (emit "   cmpl $~s, %eax" (immediate-rep 0))
      (emit-eax-to-bool)]
    ))

(define (emit-eax-to-bool)
  (emit "   sete %al") ; set byte to 1 if equal
  (emit "   movzbl %al, %eax") ; movzbl set eax high-order 24bits to zero
  (emit "   sal $~s, %al" boolshift)  ; transform the result to bool
  (emit "   or $~s, %al" bool-f))

(load "tests-1.3-req1.scm")
