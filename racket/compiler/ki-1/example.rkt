; example for kyo

tydec -> (type type-id ty)

ty -> type-id
   -> (tyfields)
   -> vec type-id
   -> (type-id -> type-id)

tyfields -> (id type-id) ...

built-in types: int bool str unit

vardec -> (def id : type v)
       -> (def id var)

fundec -> (defn (id [v : int]
                    [u : bool]) : int
               v)
       -> (def id (fn ([x : int]
                       [u : bool]) : int
                   x))

; variable construction
Array -> (vec exp ...)
Record -> (record (field exp) ...)


; variables reference
Record Reference -> (. record field)
Array Reference -> (vec-ref exp exp)

; operators

int: + - * = < > <= >=
bool: not and or
str: s= s< s>

; expressions
const -> int|bool|str

var -> exp

fun -> (fn (t) exp)
Note: natural curry support

assignment -> (set! id exp)
           -> (vec-set! exp exp exp)

sequence -> (seq
              exp ...)

If -> (if test then else)


Application -> (rator rand)
Note: natural curry support
