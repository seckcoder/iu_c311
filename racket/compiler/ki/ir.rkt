#lang racket

; Specification

(const-exp value) ; const int value
(name-exp n) ; assembly label n
(temp-exp t) ; temporary position?/temporary value in register?
(binop-exp op e1 e2) ; binary expression
(mem-exp e) ; memory in address e
(call f l) ; procedure call; l is argument list
(eseq s e) ; s is evaluated for effect. e is evaluated as result

(move-stm t e) ; evaluate e and move it to temporary t
(move-stm m e2) ; store e2's result to memory address:m
(exp-stm e) ; evaluate e and discard result
(jump-stm e) ; evaluate e to label or address, then jump to it
(jump-stm e labs) ; evaluate e to label which should be one of labs, and jump to it.
(cjump op e1 e2 then else); condicitonal jump. (op e1 e2) -> true/false
(seq s1 s2) ; sequence of stm
(label n) ; define the constant value of name n be the current machine code address.

; translate ast to ir representation.
(define (ir ast)
  (match ast
    [(Const v)
     (const-exp v)]
    [(Var v)
     ; reference memory
