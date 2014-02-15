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
(define pairmask #x03)
(define pairtag #x01)
(define cljmask #x03)
(define cljtag #x02)
(define atommask #x03)
(define atomtag #x03)
(define atomshift 3)
(define vecmask #x03)
(define vectag #x05)
(define strmask #x03)
(define strtag #x06)
; for other immediate and objects
#|(define objmask #x03)
(define objtag #x07)|#

(define registers
  '((eax . scratch)
    (ebx . preserve)
    (ecx . scratch)
    (edx . scratch)
    (esi . preserve)
    (edi . preserve)
    (ebp . preserve)
    (esp . preserve)))

(define (scratch? reg)
  (and (pair? reg)
       (eq? (cdr reg) 'scratch)))

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
      (null? x)
      (and (pair? x) (eq? (car x) 'quote))))

(define (immediate-rep x)
  (match x
    [(? fixnum?)
     (arithmetic-shift x fxshift)]
    [(? boolean?)
     (if (eq? x #t)
       bool-t
       bool-f)]
    [(? char?)
     (+ (arithmetic-shift (char->integer x) charshift)
        chartag)]
    ['() null_v]
    [`(quote ,x)
     (+ (arithmetic-shift (symbol->string x) atomshift)
        atomtag)]
    [_ (error 'immediate-rep "~a is not an immediate" x)]
    ))

(define (unop? op) (memq op '(add1 $fxadd1 sub1 $fxsub1
                                   number->char char->number
                                   fixnum?  number? char? null?
                                   boolean? not zero?
                                   car cdr pair?
                                   )))

(define (biop? op)
  (memq op '(cons
              + fx+
              - fx-
              * fx*
              = fx=
              < fx<
              <= fx<=
              > fx>
              >= fx>=
              )))



(require racket/match
         (prefix-in env: "env.rkt")
         "../../base/utils.rkt")

(define (compile-program x)
  (~> x
      parse
      (lambda (e)
        (closure-conversion e 'topdown))
      emit-program))

(define (parse x)
  (match x
    [`(let* () ,body)
      (parse `(let () ,body))]
    [`(let* ([,v0 ,e0]) ,body)
      (parse `(let ([,v0 ,e0]) ,body))]
    [`(let* ([,v0 ,e0]
             ,bind* ...) ,body)
      `(let ([,v0 ,e0])
         ,(parse `(let* ,bind* ,body)))]
    [(? immediate?) x]
    [(? symbol?) x]
    [(list (? unop? op) _) x]
    [(list (? biop? op) _ _) x]
    [`(if ,test ,then ,else) x]
    [`(let ([,v* ,e*] ...) ,body) x]
    [`(lambda (,v* ...) ,body) x]
    [`(,rator ,rand* ...)
      `(app ,rator ,@rand*)]
    [_ (error 'parse "failed:~s" x)]))

(define free
  (lambda (e)
    (match e
      [(? immediate?)
       (seteq)]
      [(? symbol? v)
       (seteq v)]
      [(list (? unop? op) v)
       (free v)]
      [(list (? biop? op) a b)
       (free-U (list a b))]
      [`(if ,test ,then ,else)
        (free-U (list test then else))]
      [`(let ((,v* ,e*) ...) ,body)
        (set-union (free-U e*)
                   (set-subtract (free body)
                                 (list->seteq v*)))]
      [`(lambda (,v* ...) ,body)
        (set-subtract (free body)
                      (list->seteq v*))]
      [`(closure ,f ,env)
        (list->seteq (hash-keys env))]
      [`(app ,rator ,rand* ...)
        (free-U (cons rator rand*))]
      [`(app-clj ,rator ,rand* ...)
        (free-U (cons rator rand*))]
      [_ (error 'free "~a not match" e)]
      )))
(define free-U
  (lambda (es)
    (foldl
      (lambda (e u)
        (set-union u (free e)))
      (seteq)
      es)))
(define subst
  (lambda (e env fvs)
    (define (subst-clj-env clj-env)
      (let loop ([fvs (hash-keys clj-env)]
                 [clj-env clj-env])
        (cond
          [(null? fvs) clj-env]
          [else
            (loop (cdr fvs)
                  (hash-set clj-env
                            (car fvs)
                            ; subst env value
                            (subst1 (car fvs))))])))
    (define (subst1 e)
      (match e
        [(? immediate?) e]
        [(? symbol? v)
         (if (set-member? fvs v)
           `(env-ref ,env ,v)
           v)]
        [(list (? unop? op) v)
         `(,op ,(subst1 v))]
        [(list (? biop? op) a b)
         `(,op ,(subst1 a)
               ,(subst1 b))]
        [`(if ,test ,then ,else)
          `(if ,(subst1 test)
             ,(subst1 then)
             ,(subst1 else))]
        [`(let ((,v* ,e*) ...) ,body)
          `(let ,(map list v* (map subst1 e*))
             ,(subst body env (set-subtract fvs
                                            (list->seteq v*))))]
        [`(lambda (,v* ...) ,body)
          `(lambda ,v*
             ,(subst body env (set-subtract fvs
                                            (list->seteq v*))))]
        [`(closure ,f ,clj-env)
          `(closure ,(subst1 f)
                    ,(subst-clj-env clj-env))]
        [`(lambda* (,v* ...) ,body)
          ; lambda* means it's already substituted
          e]
        [`(app ,rator ,rand* ...)
          `(app ,(subst1 rator) ,@(map subst1 rand*))]
        [`(app-clj ,rator ,rand* ...)
          `(app-clj ,(subst1 rator)
                    ,@(map subst1 rand*))]
        [_ (error 'subst "~a not match" e)]
        ))
    (subst1 e)))


; (load "test-closure-conversion.scm")

(define (env-from-fvs fvs)
  (make-immutable-hasheq
    (map
      (lambda (x)
        (cons x x))
      (set->list fvs))))

(define (closure-conversion e dir)
  (letrec ([cvt1 (lambda (e)
                   (match e
                     [`(lambda (,v* ...) ,body)
                       (let ([fvs (free e)]
                             [$env (gensym 'env)])
                         `(closure
                            (lambda* (,$env ,@v*)
                                     ,(subst body $env fvs))
                            ,(env-from-fvs fvs)))]
                     [`(app ,rator ,rand* ...)
                       `(app-clj ,rator ,@rand*)]
                     [_ e]
                     ))]
           [cvt-up (lambda (e)
                     (define (cvt-up1 e)
                       (match e
                         [(? immediate?)
                          e]
                         [(? symbol?)
                          e]
                         [(list (? unop? op) v)
                          `(,op ,(cvt-up v))]
                         [(list (? biop? op) a b)
                          `(,op ,(cvt-up a)
                                ,(cvt-up b))]
                         [`(if ,test ,then ,else)
                           `(if ,(cvt-up test)
                              ,(cvt-up then)
                              ,(cvt-up else))]
                         [`(let ((,v* ,e*) ...) ,body)
                           `(let ,(map list v* (map cvt-up e*))
                              ,(cvt-up body))]
                         [`(lambda (,v* ...) ,body)
                           `(lambda ,v*
                              ,(cvt-up body))]
                         [`(app ,rator ,rand* ...)
                           (let ([rator (cvt-up rator)]
                                 [rand* (map cvt-up rand*)])
                             `(app ,rator ,@rand*))]
                         [_ (error 'cvt-up "~a not match" e)]
                         ))
                     (cvt1 (cvt-up1 e)))]
           [cvt-down (lambda (e) e)]
           [cvt (lambda (e)
                  (if (eq? dir 'topdown)
                    (cvt-down e)
                    (cvt1 (cvt-up e))))])
           (cvt e)))

; (load "test-closure-conversion.scm")

(define (emit-program x)
  (emit "   .text")
  (emit-fn-header 'scheme_entry)
  (emit-preserve-reg)
  (emit "   movl %esp, %ecx") ; store esp temporarily
  ; heap : low->high
  ; stack : high->low
  (emit "   movl 12(%ecx), %ebp") ; set heap pointer
  (emit "   movl 8(%ecx), %esp") ; set stack pointer
  (emit "   pushl 4(%ecx)") ; store ctx
  ; It's an assumption that physical addresses on Intel's
  ; 32bit processors have 8-byte boundaries. So we don't
  ; need to aligh the heap address when start.
  ;(emit-align-heap) ; aligh the start address of heap
  ((emit-exp (- wordsize) (env:empty)) x)
  (emit-restore-reg)
  (emit "   ret"))
(define (emit-preserve-reg)
  (define (si-of-i i)
    (* wordsize i))
  (emit "   movl 4(%esp), %ecx") ; ctx ptr
  (let loop ([regs registers]
             [i 0])
    (cond [(null? regs)
           'ok]
          [(scratch? (car regs))
           (loop (cdr regs)
                 (add1 i))]
          [else
            (match (car regs)
              [(cons n _)
               (emit "   movl %~a, ~a(%ecx)" n (si-of-i i))
               (loop (cdr regs)
                     (add1 i))])])))

(define (emit-restore-reg)
  (define (si-of-i i)
    (* wordsize i))
  (emit "   popl %ecx") ; get ctx ptr
  (let loop ([regs registers]
             [i 0])
    (cond [(null? regs)
           'ok]
          [(scratch? (car regs))
           (loop (cdr regs)
                 (add1 i))]
          [else
            (match (car regs)
              [(cons n _)
               (emit "  movl ~a(%ecx), %~a" (si-of-i i) n)
               (loop (cdr regs)
                     (add1 i))])])))

(define (emit-fn-header lbl)
  (emit "   .globl ~a" lbl)
  (emit "   .type ~a, @function" lbl)
  (emit "~a:" lbl))

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
        ['car
         (emit "   movl -1(%eax), %eax")]
        ['cdr
         (emit "   movl ~s(%eax), %eax" (sub1 wordsize))]
        ['pair?
         (emit "   andb $~s, %al" pairmask)
         (emit "   cmpb $~s, %al" pairtag)
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
          [cons (emit-biv)
                (emit "   movl %eax, ~s(%ebp)" wordsize) ; copy b to heap
                (emit "   movl ~s(%esp), %ecx" si) ; copy a to temporary
                (emit "   movl %ecx, (%ebp)") ; copy a to heap
                (emit "   movl %ebp, %eax")
                (emit "   orl $~s, %eax" pairtag)
                (emit "   addl $~s, %ebp" (* 2 wordsize))
                ; baseptr + 2 * wordsize is 8-byte aligned.
                ]
          ))
      (define (report-not-found)
        (error 'emit-biop "~a is not a binary operator" op))
      ;(printf "emit-op ~a ~a\n" op (hash-ref op->emitter op))
      ((hash-ref op->emitter op report-not-found)))
    (define (emit-rands rands)
      ; A simple impl, I will update it
      (foldl
        (lambda (exp si)
          ((emit-exp si env) exp)
          (- si wordsize))
        si
        (reverse rands)))
    (define emit-exp1
      (lambda (exp)
        (match exp
          [(? immediate? x)
           (emit "   movl $~s, %eax" (immediate-rep x))]
          [(? symbol? v)
           ; variable
           (let ([pos (env:app env v)])
             (emit "   movl ~s(%esp), %eax" pos))]
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
          #|[`(let* ((,v* ,e*) ...) ,body)
              (match (emit-decl* si env v* e*)
                [(list si env)
                 ((emit-exp si env) body)])]
          [`(letrec ((,n* ,lambda*) ,body))
            (let* ([len (length n*)]
                   [l* (mapn (lambda (_)
                               (gen-label))
                             len)]
                   [env (env:exts env n* l*)]
                   )
              (for-each
                (lambda (l lambda-e)
                  ((emit-exp1 lambda-e) l))
                l*
                lambda*)
              (emit-exp1 body))]|#
          [`(lambda (,v* ...) ,body)
            (lambda (l)
              (emit-fn-header l)
              (let* ([v-len (length v*)]
                     [si-of-v (lambda (i)
                                ; we have ret address above %esp
                                (+ i 1))]
                     [new-env
                       (let loop ([v* v*]
                                  [i 0]
                                  [env env])
                         (cond
                           [(>= i v-len) env]
                           [else
                             (loop (cdr v*)
                                   (add1 i)
                                   (env:ext env (car v*) (si-of-v i)))]))])
                (emit-exp1 body)))]
          [`(app ,rator ,rand* ...)
            (let ([rator-lbl (env:app env rator)])
              (let ([new-si (emit-rands rand*)])
                (emit "   call ~a" rator-lbl)))]
          )))
    emit-exp1))

; eval(e) could be an address or label
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

#|(load "tests-1.3-req1.scm")
(load "tests-1.4-req.scm")
(load "tests-1.5-req1.scm")
(load "tests-1.6-req.scm")
(load "tests-1.6-opt.scm")|#

; (load "tests-1.8-opt.scm")
(load "tests-sexp.scm")
