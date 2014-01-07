During the design of module, consider the following design decision:

"""
(module m1
 (sig
  (u int)
  (f ((int) int)))
 (body
  ; module body
  ; The body is just a compound expression, it will be
  ; interpreted just like a compound exp, not more modifcation.
  (define u a)
  (set! u 4)
  (define f (lambda (v)
             (+ u v)))
  ; this expression has no effect on the whole module
  (let ((v a))
   (define f (lambda (v)
              (* u v)))
   (f v))
  (define g (lambda () u))
 ))
"""

And when we use type inference for the whole program and
check the type for module based on the interface.

For the above design, we can't implement it. Why?
The reason is hard to explain. But you will know it when you
are trying to implement it. If the body is just a compound expression,
after you unifying the expression, you don't know the output
binding variable of the compound exp, so you can't check the interface
type declaration. Therefore, the body part should not be interpreted
as a compound exp(like ml) or we should not declare types for interface(like racket)

eopl 8.5: This is a poor design for my type-infer system. It breaks the consistency of
code.
