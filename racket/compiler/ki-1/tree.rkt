(module tree racket
  (provide (all-defined-out))

  ; exp
  (struct const (v) #:transparent)
  ; assembly label
  (struct name (s) #:transparent)
  ; temporary t
  (struct temp (t) #:transparent)
  (struct biop (op a b) #:transparent)
  (struct unop (op v) #:transparent)
  ; contents of memory starting at eval(addr)
  (struct mem (addr) #:transparent)
  ; call f with args:l
  (struct call (f l) #:transparent)
  ; statement: s, expression: e
  (struct eseq (s e) #:transparent)

  ; stm
  
  ; move eval(e) to t(temporary | memory address)
  (struct move (t e) #:transparent)

  ; eval(e), discard result
  (struct exp (e) #:transparent) 

  ; jump to e:label or address
  (struct jump (e labs) #:transparent)

  ; conditional jump o(e1 e2) 
  (struct cjump (o e1 e2 t f) #:transparent)

  ; two statements
  (struct seq (s1 s2) #:transparent)

  ; define s as the label for current machine code address
  (struct label (s) #:transparent)
  )
