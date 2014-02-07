; specification for kyo

; declarations
decs -> {dec}

dec -> tydec
    -> vardec
    -> fundec

tydec -> (type type-id ty)

ty -> type-id
   -> (tyfields)
   -> (vec type-id)
   -> (type-id -> type-id)

tyfields -> (id type-id) ...

built-in types: int bool str unit

vardec -> (def id : type-id v)

fundec -> (defn (id [v : type-id] ...) : type-id
               exp)
      -> (def id (fn ([v : type-id] ...) : type-id
                   exp))

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

fun -> (fn ([v : type-id] ...) : type-id
         exp)
Note: natural curry support

assignment -> (set! id exp)
           -> (vec-set! exp exp exp)

sequence -> (seq
              exp ...)

If -> (if exp exp exp)

; We can't declare local functions(We use stack machine...)
let -> (let ([v : type-id exp])
         exp)


Application -> (exp exp)
Note: natural curry support
