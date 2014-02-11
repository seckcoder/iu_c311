(load "tests-driver.scm")

;(load "tests-1.1-req.scm")

; 1.2
;(load "tests-1.2-req.scm")

(define fxshift 2)
(define fxmask #x03)
(define fxtag #x00)
(define bool-f #x2F)
(define bool-t #x6F)
(define null_v #x3F)
(define charmask #xFF)
(define chartag #x0F)
(define charshift 8)
(define boolshift 6)
(define boolmask #x3F)
(define booltag #x2F)
(define wordsize 4) ; 4byte

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


(define (compile-program x)
  (emit "   .text")
  (emit "   .globl	scheme_entry")
  (emit "   .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "   movl %esp, %ecx") ; store os' esp in ecx
  (emit "   movl 4(%esp), %esp") ; store stack address in %esp
  ((emit-exp (- wordsize)) x)
  (emit "   movl %ecx, %esp") ; recover os' esp
  (emit "   ret"))

(require racket/match)
(define emit-exp
  (lambda (si)
    (define emit-exp1
      (lambda (exp)
        (match exp
               [(or (? number? x)
                    (? boolean? x)
                    (? char? x)
                    (? null? x))
                (emit "   movl $~s, %eax" (immediate-rep x))]
               [`(add1 ,v)
                 (emit-exp1 v)
                 (emit "   addl $~s, %eax" (immediate-rep 1))]
               [`($fxadd1 ,v)
                 (emit-exp1 `(add1 ,v))]
               [`(sub1 ,v)
                 (emit-exp1 v)
                 (emit "   subl $~s, %eax" (immediate-rep 1))]
               [`($fxsub1 ,v)
                 (emit-exp1 `(sub1 ,v))]
               [`(number->char ,v)
                 (emit-exp1 v)
                 ; shift left
                 (emit "   shll $~s, %eax" (- charshift fxshift))
                 ; change the shifted to char tag
                 (emit "   orl $~s, %eax" chartag)]
               [`(char->number ,v)
                 (emit-exp1 v)
                 ; just shift right
                 (emit "   sarl $~s, %eax" (- charshift fxshift))]
               [`(fixnum? ,v)
                 (emit-exp1 `(number? ,v))]
               [`(number? ,v)
                 (emit-exp1 v)
                 (emit "   andb $~s, %al" fxmask)
                 (emit "   cmpb $~s, %al" fxtag)
                 (emit-eax-to-bool)]
               [`(null? ,v)
                 (emit-exp1 v)
                 (emit "   cmpw $~s, %ax" null_v)
                 (emit-eax-to-bool)]
               [`(boolean? ,v)
                 (emit-exp1 v)
                 (emit "   andb $~s, %al" boolmask)
                 (emit "   cmpb $~s, %al" booltag)
                 (emit-eax-to-bool)]
               [`(char? ,v)
                 (emit-exp1 v)
                 (emit "   andb $~s, %al" charmask)
                 (emit "   cmpb $~s, %al" chartag)
                 (emit-eax-to-bool)]
               [`(not ,v)
                 (emit-exp1 v)
                 (emit "   cmpw $~s, %ax" bool-f)
                 ; if equal=#f, we set al to 1, then we transform it to #t.
                 ; if equal to other value, we set al to 0, then transformed to #f.
                 (emit-eax-to-bool)]
               [`(zero? ,v)
                 (emit-exp1 v)
                 (emit "   cmpl $~s, %eax" (immediate-rep 0))
                 (emit-eax-to-bool)]
               [`(if ,test ,then ,else)
                 (emit-exp1 test)
                 (let ((else-lbl (gen-label))
                       (endif-lbl (gen-label)))
                   ; jump to else if equal to false
                   ; Que: how to optimize this?
                   (emit "   cmpl $~s, %eax" bool-f)
                   (emit "   je ~a" else-lbl)
                   (emit-exp1 then)
                   (emit "   jmp ~s" endif-lbl)
                   (emit "~s:" else-lbl)
                   (emit-exp1 else)
                   (emit "~s:" endif-lbl))]
               [`(+ ,a ,b)
                 (emit-exp1 a)
                 (emit "   movl %eax, ~s(%esp)" si); store a to stack
                 ((emit-exp (- si wordsize)) b)
                 (emit "   addl ~s(%esp), %eax" si) ; b = a + b
                 ]
               [`(fx+ ,a ,b)
                 (emit-exp1 `(+ ,a ,b))]
               )))
    emit-exp1))

(define gen-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        (string->symbol L)))))

(define (emit-eax-to-bool)
  (emit "   sete %al") ; set byte to 1 if equal
  (emit "   movzbl %al, %eax") ; movzbl set eax high-order 24bits to zero
  (emit "   sal $~s, %al" boolshift)  ; transform the result to bool
  (emit "   or $~s, %al" bool-f))

; (load "tests-1.3-req1.scm")
; (load "tests-1.4-req.scm")
(load "tests-1.5-req1.scm")
