based on letrec-multi-declare
implicit reference(call-by-value) with recursion based on circular structure
The implementation on the book is actually wrong. Each time you call a recursive
procedure, it will allocate a new piece of memory.

The implementation in 4.19 is right.

eopl 3.35 4.19
