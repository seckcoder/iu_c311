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


(require racket/match
         (prefix-in env: "env.rkt"))

(define (compile-program x)
  (emit "   .text")
  (emit "   .globl	scheme_entry")
  (emit "   .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "   movl %esp, %ecx") ; store os' esp in ecx, ecx is used as temporary
  (emit "   movl 4(%esp), %esp") ; store stack address in %esp
  (emit "   pushl %ecx") ; store os'esp on stack
  ((emit-exp (- wordsize) (env:empty)) x)
  (emit "   popl %esp") ; recover os'esp
  (emit "   ret"))

(define (unop? op)
  (memq op '(add1 $fxadd1 sub1 $fxsub1
                  number->char char->number fixnum?
                  number? char? null? boolean? not
                  zero?)))

(define (biop? op)
  (memq op '(+ fx+
               - fx-
               * fx*
               = fx=
               < fx<
               <= fx<=
               > fx>
               >= fx>=)))

(define emit-exp
  (lambda (si env)
    (define (emit-unop op v)
      (emit-exp1 v)
      (match op
        ['add1 (emit "   addl $~s, %eax" (immediate-rep 1))]
        ['$fxadd1 (emit-exp1 `(add1 ,v))]
        ['sub1 (emit "   subl $~s, %eax" (immediate-rep 1))]
        ['$fxsub1 (emit-exp1 `(sub1 ,v))]
        ['number->char
         ; shift left
         (emit "   shll $~s, %eax" (- charshift fxshift))
         ; change the shifted to char tag
         (emit "   orl $~s, %eax" chartag)]
        ['char->number
         (emit "   sarl $~s, %eax" (- charshift fxshift))]
        ['fixnum?
         (emit-exp1 `(number? ,v))]
        ['number?
         (emit "   andb $~s, %al" fxmask)
         (emit "   cmpb $~s, %al" fxtag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['null?
         (emit "   cmpw $~s, %ax" null_v)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['boolean?
         (emit "   andb $~s, %al" boolmask)
         (emit "   cmpb $~s, %al" booltag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['char?
         (emit "   andb $~s, %al" charmask)
         (emit "   cmpb $~s, %al" chartag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['not
         (emit "   cmpw $~s, %ax" bool-f)
         ; if equal=#f, we set al to 1, then we transform it to #t.
         ; if equal to other value, we set al to 0, then transformed to #f.
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['zero?
         (emit "   cmpl $~s, %eax" (immediate-rep 0))
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        [_ (error 'emit-unop "~a is not an unary operator" op)]))
    (define (emit-biop op a b)
      (define (emit-*)
        (emit-exp1 a)
        (emit-remove-fxtag) ; remove fxtag so that it can be used in imull
        (emit "  movl %eax, ~s(%esp)" si)
        ((emit-exp (- si wordsize) env) b)
        (emit-remove-fxtag)
        (emit "  imull ~s(%esp), %eax" si) ; multiply two number
        (emit-add-fxtag))
      (define (emit-biv)
        (emit-exp1 a)
        (emit "   movl %eax, ~s(%esp)" si); store a to stack
        ((emit-exp (- si wordsize) env) b))
      (define-syntax gen-pairs
        (syntax-rules ()
          [(_)
           (list)]
          [(_ (op0 p* ...) pair* ...)
           (cons
             (cons `op0
                   (lambda ()
                     p* ...))
             (gen-pairs pair* ...))]))
      (define-syntax biop-emit-pairs
        (syntax-rules ()
          [(_ p0 p* ...)
           (make-hasheq
             (gen-pairs p0 p* ...))]))
      (define (emit-cmp op)
        (emit-biv)
        ;(printf "emit-cmp:~s\n" op)
        (emit "  cmpl ~s(%esp), %eax" si)
        (case op
          ['= (emit "   sete %al")]
          ['< (emit "   setg %al")]
          ['<= (emit "  setge %al")]
          ['> (emit "   setl %al")]
          ['>= (emit "  setle %al")]
          [else (report-not-found)])
        ;(printf "end of emit-cmp\n")
        (emit-eax-0/1->bool))
      (define op->emitter
        (biop-emit-pairs
          [* (emit-*)]
          [fx* (emit-exp1 `(* ,a ,b))]
          [+ (emit-biv)
             ; b = a + b
             (emit "   addl ~s(%esp), %eax" si)]
          [fx+ (emit-exp1 `(+ ,a ,b))]
          [- (emit-biv) 
             ; b = a - b
             (emit "   movl %eax, %ecx")
             (emit "   movl ~s(%esp), %eax" si)
             (emit "   subl %ecx, %eax")]
          [fx- (emit-exp1 `(- ,a ,b))]
          [= (emit-cmp '=)]
          [fx= (emit-exp1 `(= ,a ,b))]
          [< (emit-cmp '<)]
          [fx< (emit-exp1 `(< ,a ,b))]
          [<= (emit-cmp '<=)]
          [fx<= (emit-exp1 `(<= ,a ,b))]
          [> (emit-cmp '>)]
          [fx> (emit-exp1 `(> ,a ,b))]
          [>= (emit-cmp '>=)]
          [fx>= (emit-exp1 `(>= ,a ,b))]
          ))
      (define (report-not-found)
        (error 'emit-biop "~a is not a binary operator" op))
      ;(printf "emit-op ~a ~a\n" op (hash-ref op->emitter op))
      ((hash-ref op->emitter op report-not-found)))
    (define emit-exp1
      (lambda (exp)
        (match exp
          [(or (? number? x)
               (? boolean? x)
               (? char? x)
               (? null? x))
           (emit "   movl $~s, %eax" (immediate-rep x))]
          [(? symbol? v)
           ; variable
           (let ([pos (env:app env v)])
             (emit "  movl ~s(%esp), %eax" pos))]
          [(list (? unop? op) v)
           (emit-unop op v)]
          [(list (? biop? op) a b)
           (emit-biop op a b)]
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
          [`(let ((,v* ,e*) ...) ,body)
            (match (emit-decls si env v* e*)
              [(list si env)
               ((emit-exp si env)
                body)])]
          [`(let* ((,v* ,e*) ...) ,body)
            (match (emit-decl* si env v* e*)
              [(list si env)
               ((emit-exp si env) body)])]
          )))
    emit-exp1))

(define (emit-decl si env v e)
  ((emit-exp si env) e)
  (emit "   movl %eax, ~s(%esp)" si)
  (list (- si wordsize)
        (env:ext env v si)))

; for let
(define (emit-decls si env vs es)
  (let loop [(si si)
             (cur-vs vs)
             (cur-es es)
             (si-acc '())]
    (cond
      [(and (null? cur-vs)
            (null? cur-es))
       (list si
             (env:exts env vs (reverse si-acc)))]
      [(or (null? cur-vs)
           (null? cur-es))
       (error 'emit-decls "vs and es have different length")]
      [else
        ((emit-exp si env) (car cur-es))
        (emit "   movl %eax, ~s(%esp)" si)
        (loop (- si wordsize)
              (cdr cur-vs)
              (cdr cur-es)
              (cons si si-acc))])))
  

; for let*
(define (emit-decl* si env vs es)
  (foldl
    (match-lambda*
      [(list v e (list si env))
       (emit-decl si env v e)])
    (list si env)
    vs
    es))

(define (emit-remove-fxtag)
  (emit "   sar $~s, %eax" fxshift))

(define (emit-add-fxtag)
  (emit "   sal $~s, %eax" fxshift))

(define gen-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        (string->symbol L)))))

; after cmp operation, we can set eax to bool value according
; to the flags
(define (emit-eax-0/1->bool)
  (emit "   movzbl %al, %eax") ; movzbl set eax high-order 24bits to zero
  (emit "   sal $~s, %al" boolshift)  ; transform the result to bool
  (emit "   or $~s, %al" bool-f))

; (load "tests-1.3-req1.scm")
; (load "tests-1.4-req.scm")
; (load "tests-1.5-req1.scm")
(load "tests-1.6-req.scm")
(load "tests-1.6-opt.scm")
