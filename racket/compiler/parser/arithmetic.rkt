; parsing of integer operation +/*

The most simple and naive declaration for the grammar:
Exp = int
    | (Exp)
    | Exp + Exp
    | Exp * Exp

But this is not enough, it can cause ambiguity

Fix 1: modify grammar

Exp = Exp1 + Exp
    | Exp1

Exp1 = int
     | int * Exp1
     | (Exp)

But this approach is too hard(Consider when the grammar is very
complex)

Fix 2: set associativity and precedence

Exp = int
    | (Exp)
    | Exp + Exp ; + has higer precedence than *. So in 1+2*3, 1+2 will first be matched
    | Exp * Exp

(+ : left)
-> + is left associative. That's, 1 + 2 + 3 will be parsed into (1 + 1) + 3
(* : left)

Now we can parse it.

But we also want to change it into LL(1), since we didn't need to backtrack.

Fix 3: to LL(1) by changing the syntax

Exp = int
    | * Exp Exp ; * has higher precedence
    | + Exp Exp
    | (Exp)

Example:
+ * 1 2 + 3 4  ; We even don't need bracket

Fix 4: how about not change the syntax?
...
