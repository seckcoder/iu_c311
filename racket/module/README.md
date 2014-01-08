type.rkt
========
This is an improved version of type inference. It differs from `types/type-infer1.rkt
in following aspects:
- It adds the support of module type
- The list type is different. For `type/type-infer1.rkt`,
    """
     (lambda (v)
        (cons v '()))
    """
  has type `((tv) -> list). The type is not right. Since it also accepts
  value:`(list 2 . 3)`. In this type system, the type is:`((tv) -> (tv))`
  or `((tv) -> (Pair tv (Nil)))
- Fix some bugs

eopl 8.5: This is a poor design for my type-infer system. It breaks the consistency of
code.
